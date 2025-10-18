module Zipper

type Tree<'a> = { Value: 'a; Left: Tree<'a> option; Right: Tree<'a> option }

type Crumb<'a> = | Left of 'a * Tree<'a> option | Right of 'a * Tree<'a> option

type Zipper<'a> = { Focus: Tree<'a>; Context: Crumb<'a> list }

let tree (value: 'a) (left: Tree<'a> option) (right: Tree<'a> option): Tree<'a> = { Value = value; Left = left; Right = right }

let value (zipper: Zipper<'a>): 'a = zipper.Focus.Value

let setValue (newValue: 'a) (z: Zipper<'a>): Zipper<'a> = { z with Focus = { z.Focus with Value = newValue } }

let setLeft (newLeft: Tree<'a> option) (z: Zipper<'a>): Zipper<'a> = { z with Focus = { z.Focus with Left = newLeft } }

let setRight (newRight: Tree<'a> option) (z: Zipper<'a>): Zipper<'a> = { z with Focus = { z.Focus with Right = newRight } }

let fromTree (t: Tree<'a>): Zipper<'a> = { Focus = t; Context = [] }

let left (zipper: Zipper<'a>): Zipper<'a> option =
    match zipper.Focus.Left with
    | None -> None
    | Some left -> Some { Focus = left; Context = Left (zipper.Focus.Value, zipper.Focus.Right) :: zipper.Context }

let right (zipper: Zipper<'a>): Zipper<'a> option =
    match zipper.Focus.Right with
    | None -> None
    | Some right -> Some { Focus = right; Context = Right (zipper.Focus.Value, zipper.Focus.Left) :: zipper.Context }

let up (zipper: Zipper<'a>): Zipper<'a> option =
    match zipper.Context with
    | [] -> None
    | crumb :: rest ->
        match crumb with
        | Left (v, r) -> Some { Focus = tree v (Some zipper.Focus) r; Context = rest }
        | Right (v, l) -> Some { Focus = tree v l (Some zipper.Focus); Context = rest }

let toTree (zipper: Zipper<'a>): Tree<'a> =
    let rec go (z: Zipper<'a>) =
        match z.Context with
        | [] -> z.Focus
        | crumb :: rest ->
            match crumb with
            | Left (v, r) -> go { Focus = tree v (Some z.Focus) r; Context = rest }
            | Right (v, l) -> go { Focus = tree v l (Some z.Focus); Context = rest }
    go zipper
