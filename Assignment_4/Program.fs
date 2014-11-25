#light

type Name = string

type Sex = 
    | M
    | F

type YearOfBirth = int

type FamilyTree = P of Name * Sex * YearOfBirth * Children
and Children = FamilyTree list


// 1. Every person must be older than his/her children
// 2. For every list of children, the siblings occur with decreasing 
//    ages so that the eldest occurs first and the youngest last.

let getYear = function
    | P(_,_,year,_) -> year

let getName = function
    | P(name,_,_,_) -> name

let rec isWF = function
    | P(_, _, year, children) -> 
        children |> Seq.pairwise |> Seq.forall (fun (a, b) -> getYear a <= getYear b)
        && children |> List.forall(fun x -> getYear x > year && isWF x)

let makePerson (name, sex, year) = P(name, sex, year, [])
   
let rec insertChildOf n c t =
    match t with
    | P(name, sex, year, children) ->
        if name=n then 
            let temp = P(name, sex, year, c::children)
            if isWF(temp) then Some(temp) else None
        else 
            match insertChildOfInList n c children with 
            | Some(x) ->
                let temp = P(name, sex, year, x)
                if isWF(temp) then Some(temp) else None
            | None -> None
and insertChildOfInList n c children =
    match children with
    | [] -> None
    | head::tail -> 
        match insertChildOf n c head with 
        | Some(x) -> Some(x::tail)
        | None -> 
            match insertChildOfInList n c tail with
            | Some(y) -> Some(head::y)
            | None -> None

let children_info children = 
    List.fold (fun acc x -> (getName x)::acc) [] children

let rec find n t = 
    match t with
    | P(name, sex, year, children) ->
        if name=n then Some((sex, year, children_info children))
        else find_list n children
and find_list n c =
    match c with
    | [] -> None
    | head::tail -> 
        match find n head with
        | Some(x) -> Some(x)
        | None -> find_list n tail
        

let MaryChildren = [
    P("Peter", M, 2005, []);
    P("Bob", M, 2008, []);
    P("Eve", F, 2010, [])]
let Marry = P("Marry", F, 1980, MaryChildren)

let MayChildren = [
    P("Fred", M, 1970, []);
    P("Joan", F, 1975, [])]
let May = P("May", F, 1945, MayChildren)

let JoeChildren = [
    P("Stanley", M, 1975, []);
    Marry;
    P("Jane", F, 1985, [])]
let Joe = P("Joe", M, 1950, JoeChildren)

let LarryChildren = [
    May;
    Joe;
    P("Paul", M, 1955, [])]
let Larry = P("Larry", M, 1920, LarryChildren)

let isWFTest = isWF Larry
if isWFTest then printfn "ok" else printfn "error"

let old_person = makePerson("Leopold", M, 1940)
let young_person = makePerson("Mirka", F, 1990)

let new_family = insertChildOf "Paul" old_person Larry
let new_family_2 = insertChildOf "Paul" young_person Larry

let info = find "Marry" Larry
let info_2 = find "Mirka" new_family_2.Value
let info_3 = find "Karol" Larry 
