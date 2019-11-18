// WHQW401 Exam 2015
//=================================================================
//1. Theory
//=================================================================
//----------------------------------------------------------------
// Question 1.1.1
//----------------------------------------------------------------
// Prove using induction that Elem n (xs @ ys) = Elem n xs || Elem n ys
//
// Step 1: Prove for xs = []
// LHS: Element n ([] @ ys) = Element n ys 
// RHS: count [] + coount ys = 0 + count ys = count ys (code line 3)
// Thus LHS = RHS
//
// Step 2: Assume that the theorem holds for xs 
// Thus Elem n (xs @ ys) = Elem n xs || Elem n ys
//
// Step 3: Prove that theorem holds for x::xs
// r.t.p. Elem n (x::xs @ ys) = Elem n x::xs || Elem n ys
//
// LHS: Elem n (x::xs @ ys) = Elem n x::(xs @ ys)
//      = Elem n (xs @ ys) (code line 5)
//      = Elem n xs || Elem n ys
//
// RHS: Elem n x::xs || Elem n ys = Elem n xs || Elem n ys (code line 5) 
//
// Thus LHS = RHS
//
// Thus theorem has been proved using induction
//
//----------------------------------------------------------------
// Question 1.1.2
//----------------------------------------------------------------
//
// Evaluate (((λf.λy.λx(f (y x)) λs.(s s)) λa.λb.b) λx.λy.x )
//
//          = ((λy.λx(λx.λy.x (y x))) λa.λb.b)
//          = (λx.λy.x ( λs.(s s)  λa.λb.b))
//          =  λs.(s s)
//
//=================================================================
//2. Practical 
//=================================================================
//----------------------------------------------------------------
// Question 2.1.1
//----------------------------------------------------------------
let py = [for a in 1.0 .. 100.0 do 
            for b in 1.0 .. 100.0 do 
                for c in 1.0 .. 100.0 do if (a**2.0 + b**2.0 = c**2.0) then yield (a,b,c)]
//----------------------------------------------------------------
// Question 2.1.2
//----------------------------------------------------------------
let rec calcPascal list result = 
    match list with
    | [] -> result
    | e1::e2::tail when tail <> [] -> calcPascal (e2::tail) ((e1+e2)::result)
    | e1::e2::tail when tail = [] -> calcPascal [] (1::(e1 + e2)::result)


let rec PascalTriangle n = 
    match n with
    |0 -> []
    |1 -> [1]
    |2 -> [1;1]
    |n -> calcPascal (PascalTriangle(n-1)) [1]

PascalTriangle 6
//----------------------------------------------------------------
// Question 2.1.3
//----------------------------------------------------------------
// first create function that will sort the list 

let rec insertSort item list buffer = 
    match list with 
    | [] -> buffer @ [item]
    | head::tail when item < head -> buffer @ item::head::tail
    | head:: tail -> insertSort item tail (buffer @ [head])

let sort list = 
    let rec sortHelper list sortedList = 
        match list with
        | [] -> sortedList
        | head::tail -> sortHelper tail (insertSort head sortedList [])

    sortHelper list []

//now create function to check if two lists are permutations of each other

let checkIfPermutations list1 list2 = 
    let rec checkHelper xs ys result = 
        match xs, ys with
        | [], [] -> result
        | xs, [] -> false //since then lists are not the same
        | [], ys -> false 
        | x::xs, y::ys -> if x = y then checkHelper xs ys true 
                                   else false

    checkHelper (sort list1) (sort list2) true

checkIfPermutations [1;3;4;2] [2;3;4;2]
