module Gigasecond
open System

let gs = 1_000_000_000

let add (beginDate : DateTime): DateTime = beginDate.AddSeconds gs