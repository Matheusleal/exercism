module BirdWatcher

let lastWeek: int[] = [| 0; 2; 5; 3; 7; 8; 4 |]

let yesterday(counts: int[]): int = if counts.Length > 1 then counts[counts.Length - 2] else 0

let total(counts: int[]): int = Array.sumBy id counts

let dayWithoutBirds(counts: int[]): bool = Array.contains 0 counts

let incrementTodaysCount(counts: int[]): int[] = 
  let newArr = if Array.isEmpty counts then [|0|] else counts
  newArr[newArr.Length - 1] <- newArr[newArr.Length - 1] + 1
  newArr

let unusualWeek(counts: int[]): bool =
  match counts with
  | [|_; 0; _; 0; _; 0; _|] -> true
  | [|_; 10; _; 10; _; 10; _|] -> true
  | [|5; _; 5; _; 5; _; 5|] -> true
  | _ -> false