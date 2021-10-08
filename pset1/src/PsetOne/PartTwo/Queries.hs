{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleContexts      #-}

module PsetOne.PartTwo.Queries where

import           PsetOne.PartTwo.Schema
import           PsetOne.Util

import           Data.Int
import           Data.Maybe
import           Database.Beam

-- helper functions for red boats
isRed b = _boatColor b ==. val_ "red"
redBs = filter_ isRed boats
nonRedBs = filter_ (not_ . isRed) boats

joinOnBid b r = _reservesBid r `references_` b
joinOnSid s r = _reservesSid r `references_` s

-- sailors, boats, and reservations abbreviated as s, b, and r, respectively

pset1Query1 =
  runQuery
    $ runSelectReturningList
    $ select
    $ orderBy_ (\(BoatId bid, _, _) -> asc_ bid)
    $ join
        boats
        (aggregate_ (\(_, r) -> (group_ (_reservesBid r), as_ @Int32 countAll_))
        $ join boats reserves joinOnBid (,)
        )
        (\b (bid, _) -> bid `references_` b)
        (\b (bid, count) -> (bid, (_boatBname b), count))

pset1Query2 = runQuery $ runSelectReturningList $ select $ do
  s <- filter_
    (\s -> not_ . exists_ $ filter_
      (\b ->
        isRed b
          &&. (not_ . exists_ $ filter_
                (\r -> joinOnBid b r &&. joinOnSid s r)
                reserves
              )
      )
      boats
    )
    sailors
  pure (pk s, _sailorSname s)

pset1Query3 = runQuery $ runSelectReturningList $ select $ join
  sailors
  (let haveReservedRed = join redBs reserves joinOnBid (\_ r -> _reservesSid r)
       haveReservedNonRed =
         join nonRedBs reserves joinOnBid (\_ r -> _reservesSid r)
   in  haveReservedRed `except_` haveReservedNonRed
  )
  (\s sid -> sid `references_` s)
  (\s sid -> (sid, _sailorSname s))

pset1Query4 = runQuery $ runSelectReturningList $ selectWith $ do
  reservesByB <- selecting $ aggregate_
    (\r -> (group_ (_reservesBid r), as_ @Int32 countAll_))
    reserves
  pure $ filter_
    (\(_, _, count) -> count ==. fromMaybe_
      0
      (subquery_ $ aggregate_ (\(_, count) -> max_ count) $ reuse reservesByB)
    )
    (join (reuse reservesByB)
          boats
          (\(bid, _) b -> bid `references_` b)
          (\(bid, count) b -> (bid, _boatBname b, count))
    )

pset1Query5 = runQuery $ runSelectReturningList $ select $ do
  s <- nub_ $ except_
    sailors
    (join redBs
          (join sailors reserves joinOnSid (,))
          (\b (_, r) -> joinOnBid b r)
          (\_ (s, _) -> s)
    )
  pure (pk s, _sailorSname s)

pset1Query6 = do
  result <- runQuery $ runSelectReturningOne $ select $ do
    count <- aggregate_ (\s -> avg_ $ cast_ (_sailorAge s) double)
      $ filter_ (\s -> _sailorRating s ==. val_ 10) sailors
    pure $ fromMaybe_ 0 count
  pure $ fromMaybe 0 result

pset1Query7 =
  runQuery
    $ runSelectReturningList
    $ select
    $ orderBy_ (\(_, _, rating, _) -> asc_ rating)
    $ join
        sailors
        (aggregate_ (\s -> (group_ (_sailorRating s), min_ (_sailorAge s)))
                    sailors
        )
        (\s (rating, age) ->
          (_sailorRating s ==. rating)
            &&. (_sailorAge s ==. fromMaybe_ (-1) age)
        )
        (\s _ -> (pk s, _sailorSname s, _sailorRating s, _sailorAge s))

pset1Query8 = runQuery $ runSelectReturningList $ selectWith $ do
  rCountByB <-
    selecting
    $ aggregate_ (\(sid, bid) -> (group_ bid, group_ sid, as_ @Int32 countAll_))
    $ join boats reserves joinOnBid (\_ r -> (_reservesSid r, _reservesBid r))
  pure $ orderBy_ (\(BoatId bid, _, _, _) -> asc_ bid) $ join
    (join
      ( aggregate_ (\(bid, _, count) -> (group_ bid, max_ count))
      $ reuse rCountByB
      )
      (reuse rCountByB)
      (\(bid1, count1) (bid2, _, count2) ->
        (bid1 ==. bid2) &&. (fromMaybe_ 0 count1 ==. count2)
      )
      (flip const)
    )
    sailors
    (\(_, sid, _) s -> sid `references_` s)
    (\(bid, sid, count) s -> (bid, sid, _sailorSname s, count))
