{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Queries where

import           Schema
import           Util

import           Database.Beam

pset1Query1 = runQuery $ runSelectReturningList $ select $ join
  boats
  ( aggregate_
      (\(boat, reservation) ->
        (group_ (_reservesBid reservation), as_ @Int countAll_)
      )
  $ join boats
         reserves
         (\boat reservation -> _reservesBid reservation `references_` boat)
         (,)
  )
  (\boat (bid, count) -> bid `references_` boat)
  (\boat (bid, count) -> (bid, (_boatBname boat), count))

pset1Query2 = runQuery $ runSelectReturningList $ select $ do
  sailor <- filter_
    (\sailor -> not_ . exists_ $ filter_
      (\boat ->
        (_boatColor boat ==. val_ "red")
          &&. (not_ . exists_ $ filter_
                (\reservation ->
                  (_reservesBid reservation `references_` boat)
                    &&. (_reservesSid reservation `references_` sailor)
                )
                reserves
              )
      )
      boats
    )
    sailors
  pure (pk sailor, _sailorSname sailor)

pset1Query3 = runQuery $ runSelectReturningList $ select $ join
  sailors
  (let haveReservedRed = join
         reserves
         (filter_ (\boat -> _boatColor boat ==. val_ "red") boats)
         (\reservation boat -> _reservesBid reservation `references_` boat)
         (\reservation boat -> (_reservesSid reservation))
       haveReservedNonRed = join
         reserves
         (filter_ (\boat -> _boatColor boat /=. val_ "red") boats)
         (\reservation boat -> _reservesBid reservation `references_` boat)
         (\reservation boat -> (_reservesSid reservation))
   in  haveReservedRed `except_` haveReservedNonRed
  )
  (\sailor sid -> sid `references_` sailor)
  (\sailor sid -> (sid, _sailorSname sailor))

pset1Query4 = runQuery $ do
  runSelectReturningList
    $ select
    $ (let reservesByBoat = aggregate_
             (\reservation ->
               (group_ (_reservesBid reservation), as_ @Int countAll_)
             )
             reserves
           reservesByBoat2 = aggregate_
             (\reservation ->
               (group_ (_reservesBid reservation), as_ @Int countAll_)
             )
             reserves
           maximumReservesByBoat = filter_
             (\(_, _, count) -> count ==. fromMaybe_
               0
               ( subquery_
               $ aggregate_ (\(_, count) -> max_ count) reservesByBoat
               )
             )
             (join reservesByBoat2
                   boats
                   (\(bid, count) boat -> bid `references_` boat)
                   (\(bid, count) boat -> (bid, _boatBname boat, count))
             )
       in  maximumReservesByBoat
      )

pset1Query5 = runQuery $ runSelectReturningList $ select $ do
  sailor <- nub_ $ except_
    sailors
    (join
      (filter_ (\boat -> _boatColor boat ==. val_ "red") boats)
      (join
        sailors
        reserves
        (\sailor reservation -> _reservesSid reservation `references_` sailor)
        (,)
      )
      (\boat (_, reservation) -> _reservesBid reservation `references_` boat)
      (\_ (sailor, _) -> sailor)
    )
  pure (pk sailor, _sailorSname sailor)

pset1Query6 =
  runQuery
    $ runSelectReturningList
    $ select
    -- need to cast `age` to fp type otherwise it would return an int
    $ aggregate_ (\sailor -> avg_ $ cast_ (_sailorAge sailor) double)
    $ filter_ (\sailor -> _sailorRating sailor ==. val_ 10) sailors

pset1Query7 = runQuery $ runSelectReturningList $ select $ join
  sailors
  (aggregate_
    (\sailor -> (group_ (_sailorRating sailor), min_ (_sailorAge sailor)))
    sailors
  )
  (\sailor (rating, age) ->
    (_sailorRating sailor ==. rating)
      &&. (_sailorAge sailor ==. fromMaybe_ (-1) age)
  )
  (\sailor _ ->
    (pk sailor, _sailorSname sailor, _sailorRating sailor, _sailorAge sailor)
  )

pset1Query8 = do
  runQuery
    $ runSelectReturningList
    $ select
    $ (let
         reservationCountByBoat =
           aggregate_
               (\(sid, bid) -> (group_ bid, group_ sid, as_ @Int countAll_))
             $ join
                 boats
                 reserves
                 (\boat reservation ->
                   _reservesBid reservation `references_` boat
                 )
                 (\boat reservation ->
                   (_reservesSid reservation, _reservesBid reservation)
                 )
         maxReservationsByBoat = aggregate_
           (\(bid, sid, count) -> (group_ bid, max_ count))
           reservationCountByBoat
         reservationCountByBoat2 =
           aggregate_
               (\(sid, bid) -> (group_ bid, group_ sid, as_ @Int countAll_))
             $ join
                 boats
                 reserves
                 (\boat reservation ->
                   _reservesBid reservation `references_` boat
                 )
                 (\boat reservation ->
                   (_reservesSid reservation, _reservesBid reservation)
                 )
         maxReservationsByBoatUsers = join
           maxReservationsByBoat
           reservationCountByBoat2
           (\(bid1, count1) (bid2, _, count2) ->
             (bid1 ==. bid2) &&. (fromMaybe_ 0 count1 ==. count2)
           )
           (\_ a -> a)
         maxReservationsByBoatUserDetails = join
           maxReservationsByBoatUsers
           sailors
           (\(bid, sid, count) sailor -> sid `references_` sailor)
           (\(bid, sid, count) sailor -> (bid, sid, _sailorSname sailor, count))
       in
         maxReservationsByBoatUserDetails
      )
