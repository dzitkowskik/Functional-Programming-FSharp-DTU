#light

//////////////////////////////////////////////////////
/////////////   Propositional Logic     //////////////
//////////////////////////////////////////////////////

type Prop = 
    | P of bool
    | Dis of Prop * Prop
    | Con of Prop * Prop
    | Neg of Prop


let rec toNegationNormal p = 
    match p with
    | Neg(r) -> 
        match r with
        | Neg(s) -> toNegationNormal s
        | Con(s,t) -> Dis(toNegationNormal(Neg(s)),toNegationNormal(Neg(t)))
        | Dis(s,t) -> Con(toNegationNormal(Neg(s)),toNegationNormal(Neg(t)))
        | P(s) -> Neg(r)
    | Dis(s,t) -> Dis(toNegationNormal s, toNegationNormal t)
    | Con(s,t) -> Con(toNegationNormal s, toNegationNormal t)
    | P(s) -> P(s)


let rec toDisNorm p = 
    match p with
    | P(s) -> p
    | Neg(s) -> p
    | Con(s,t) -> 
        match (s,t) with
        | (r,Dis(x,y)) | (Dis(x,y),r) -> 
            let x_norm = toDisNorm(x)
            let y_norm = toDisNorm(y)
            let r_norm = toDisNorm(r)
            Dis(toDisNorm(Con(x_norm,r_norm)),toDisNorm(Con(y_norm,r_norm)))
        | _ -> p
    | Dis(s,t) -> Dis(toDisNorm(s), toDisNorm(t))


let rec compareProps = function
    | (P(s), P(t)) -> if s = t then true else false
    | (Dis(s1, t1), Dis(s2, t2)) -> compareProps (s1,s2) && compareProps (t1,t2)
    | (Con(s1, t1), Con(s2, t2)) -> compareProps (s1,s2) && compareProps (t1,t2)
    | (Neg(s), Neg(t)) -> compareProps (s,t)
    | (_,_) -> false


let rec litOf = function
    | Con(s, t) -> Set.union (litOf s) (litOf t) 
    | x -> Set.singleton x


let rec dnfToSet = function
    | Dis(s, t) -> Set.union (dnfToSet s) (dnfToSet t)
    | Con(s, t) -> Set.singleton (Set.union (litOf s) (litOf t))
    | x -> Set.singleton (Set.singleton x)


let isConsistent lit =
    lit 
    |> Set.exists (fun x ->
        match x with
        | P(s) -> lit.Contains(Neg(x)) 
        | Neg(s) -> lit.Contains(s)
        | _ -> false)
    |> not


let removeInconsistent dns = 
    dns |> Set.filter (fun x -> isConsistent x)



// TESTS
let t = P(true)
let f = P(false)
let test1 = toNegationNormal(Neg(Con(t, f)))
let test2 = toNegationNormal(Neg(Dis(t, t)))
let test3 = toNegationNormal(Con(Dis(Neg(Dis(Con(Neg(t),f),Neg(Dis(t,f)))),t),f))
let expected3 = Con(Dis(Con(Dis(t,Neg(f)),Dis(t,f)),t),f)
let test4 = toDisNorm test3

let expected4 = 
    Dis(
        Dis(
            Dis(
                Con(Con(t,t),f),
                Con(Con(Neg(f),t),f)),
            Dis(
                Con(Con(t,f),f),
                Con(Con(Neg(f),f),f))),
        Con(t,f))

let result1 = compareProps (test4, expected4)
assert result1
let test5 = dnfToSet test4 
let expected5 =  Set.ofList [Set.ofList [f; t]; Set.ofList [f; t; Neg (f)]; Set.ofList [f; Neg (f)]]
let result5 = (test5 = expected5)
assert result5
let set1 = Set.ofList [t; Neg(f); f]
let set2 = Set.ofList [f; t; Neg(t)]
let set3 = Set.ofList [t; Neg(f)]
let test6 = isConsistent set1
let test7 = isConsistent set2
let test8 = isConsistent set3
let expected6 = false
let expected7 = false
let expected8 = true
let result6 = (expected6 = test6)
let result7 = (expected7 = test7)
let result8 = (expected8 = test8)
assert result6
assert result6
assert result6
let test9 = removeInconsistent test5