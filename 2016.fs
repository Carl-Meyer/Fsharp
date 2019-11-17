// WHQW401 Exam 2016
//=================================================================
//1. Theory
//=================================================================
//----------------------------------------------------------------
// Question 1.1.1
//----------------------------------------------------------------
// Prove using induction that count (xs @ ys) = count xs + count ys
//
// Step 1: Prove for xs = []
// LHS: count ([] @ ys) = count ys (axiom 1)
// RHS: count [] + coount ys = 0 + count ys = count ys (code line 3)
// Thus LHS = RHS
//
// Step 2: Assume that the theorem holds for xs 
// Thus count (xs @ ys) = count xs + count ys
//
// Step 3: Prove that theorem holds for x::xs
// r.t.p. count (x::xs @ ys) = count x::xs + count ys
// LHS: count (x::xs @ ys) = count x::(xs @ ys) (axiom 4)
//      = 1 + count (xs @ ys) (code line 4)
//      = 1 + count xs + count ys (induction assumption in step 2) 
//
// RHS: count x::xs + count ys = 1 + count xs + count ys (code line 4) 
//
// Thus LHS = RHS
//
// Thus theorem has been proved using induction
//
//=================================================================
//2. Practical 
//=================================================================
//----------------------------------------------------------------
// Question 2.1.1
//----------------------------------------------------------------

let MyList = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g']
let MyList2 = ['h'; 'i'; 'j'; 'k'; 'l']


//a. Interleave with backward recursion
let rec interleaveB xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x::xs, y::ys -> x::y::interleaveB xs ys

//b. Interleave with backward recursion
let interleaveF xs ys = 
    let rec helperLeave xs ys result = 
        match xs, ys with
        | [], ys -> result::ys
        | xs, [] -> result::xs
        | x::xs, y::ys -> helperLeave xs ys result@[x;y]

    helperLeave xs ys []

//----------------------------------------------------------------
// Question 2.1.2
//----------------------------------------------------------------

//----------------------------------------------------------------
// Question 2.2
//----------------------------------------------------------------
//=== 1 ===
type character = {
    name : string;
    family: string;
    season_died: int
}

//=== 2 ===
let joffrey = {
    name = "Joffrey";
    family = "Baratheon";
    season_died = 4;
}
let Tywin = {
    name = "Tywin";
    family = "Lannister";
    season_died = 3;
}
let Rob = {
    name = "Rob";
    family = "Stark";
    season_died = 1;
}

let clist = [Tywin; Rob; joffrey]

//=== 3 ===
let insertCharacter (c : character) (list : character list) = 
    let rec insertHelper (c : character) (list : character list) (result: character list) =
        match list with
        |[] -> result @ [c]
        |x::xs -> if c.season_died > x.season_died then insertHelper c xs (result@[x])
                    else result@[c]@[x]@xs

    insertHelper c list []

insertCharacter joffrey clist
                    
// === 4 ===

let insertionSort (list : character list) = 
    let rec SortHelper list sortedList = 
        match list with
        |[] -> sortedList
        |x::xs -> SortHelper xs (insertCharacter x sortedList)
    SortHelper list []

insertionSort clist

// === 5 ===
type family = {Name: string; Deaths: int}

let f1 = {Name = "Baratheon"; Deaths = 0}
let f2 = {Name = "Lannister"; Deaths = 0}
let f3 = {Name = "Targaryen"; Deaths = 0}
let f4 = {Name = "Stark"; Deaths = 0}
let f5 = {Name = "Martel"; Deaths = 0}
   
let families = [f1;f2;f3;f4;f5]

let deathsPerFamily list =
    let rec traverse deaths families = 
        match (deaths: character list), (families : family list) with
        | [], _ -> families
        | (d::ds), (f::fs) when d.family = f.Name -> traverse ds ({f with Deaths = f.Deaths + 1}::fs)
        | (d::ds), (f::fs) -> traverse (d::ds) fs @ [f]

    traverse list families
    
//----------------------------------------------------------------
// Question 2.3
//----------------------------------------------------------------
// === 1 ===
type QuadTree = 
    |Empty
    |Point of int * int
    |Corner of QuadTree * QuadTree * QuadTree * QuadTree

// === 2 ===
let cornerA = Corner(Empty, Point(70, 20), Empty, Point(71,8))

let cornerB = Corner(Empty,Empty, cornerA, Empty)

let cornerC = Corner(Point(5,96), Empty, Empty, Point(20,70))

let quadT = Corner(cornerC, Empty, Point(20,10), cornerB)

// === 3 ===

let findPointsWithDepth quadTree = 
    let rec traverse quadTree depth =
        match quadTree with
        |Empty -> []
        |Point(x,y) -> [((x, y), depth)]
        |Corner(c1, c2, c3, c4) -> traverse c1 (depth + 1) @ traverse c2 (depth + 1) @ traverse c3 (depth + 1) @ traverse c4 (depth + 1)

    traverse quadTree 0

findPointsWithDepth quadT
