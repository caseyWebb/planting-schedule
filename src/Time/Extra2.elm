module Time.Extra2 exposing
    ( DateRange
    , addWeeks
    , firstDayOfMonth
    , firstDayOfYear
    , inDateRange
    , lastDayOfMonth
    , lastDayOfYear
    , monthNameShort
    , months
    , shiftDateRangeWeeks
    , subWeeks
    , toDayOfYear
    )

import List as List
import List.Extra as List
import Time exposing (..)
import Time.Extra as Time
import Tuple.Extra2 as Tuple


type alias DateRange =
    ( Posix, Posix )


months : List Month
months =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


monthNameShort : Month -> String
monthNameShort month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


toDayOfYear : Posix -> Int
toDayOfYear time =
    (List.takeWhile ((/=) (Time.toMonth Time.utc time)) months
        |> List.map (Time.daysInMonth 1970)
        |> List.sum
    )
        + Time.toDay Time.utc time


addWeeks : Int -> Posix -> Posix
addWeeks weeks =
    Time.addDays (weeks * 7)


subWeeks : Int -> Posix -> Posix
subWeeks weeks =
    addWeeks -weeks


firstDayOfYear : Posix
firstDayOfYear =
    Time.fromDateTuple Time.utc ( 1970, Jan, 1 )


lastDayOfYear : Posix
lastDayOfYear =
    Time.fromDateTuple Time.utc ( 1970, Dec, 31 )


firstDayOfMonth : Month -> Posix
firstDayOfMonth month =
    Time.fromDateTuple Time.utc ( 1970, month, 1 )


lastDayOfMonth : Month -> Posix
lastDayOfMonth month =
    Time.fromDateTuple Time.utc ( 1970, month, Time.daysInMonth 1970 month )


inDateRange : DateRange -> Posix -> Bool
inDateRange ( start, end ) now =
    Time.compare start now /= GT && Time.compare now end /= GT


shiftDateRangeWeeks : Int -> DateRange -> List DateRange
shiftDateRangeWeeks weeks span =
    let
        shifted =
            Tuple.mapSame (subWeeks weeks) span

        ( startDay, endDay ) =
            Tuple.mapSame toDayOfYear shifted
    in
    if startDay > endDay then
        [ ( firstDayOfYear, Tuple.second shifted )
        , ( Tuple.first shifted, lastDayOfYear )
        ]

    else
        [ shifted ]
