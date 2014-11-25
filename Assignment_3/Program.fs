#light

//////////////////////////////////////////
//                                      //
//  Karol Dzitkowski s142246 DTU        //
//  03.11.2014                          //
//  k.dzitkowski@gmail.com              //
//                                      //
//////////////////////////////////////////

module Assignment_3 =

    //////////////////////////////////////////////////////
    /////////////   Propositional Logic     //////////////
    //////////////////////////////////////////////////////

    (*
    1. You should define a type Prop for propositions so that the following are values of type Prop:
    • A "p" represents the atom p,
    • Dis(A "p", A "q") represents the proposition p ∨ q. 
    • Con(A "p", A "q") represents the proposition p ∧ q. 
    • Neg(A "p") represents the proposition ¬p.
    *)

    type Prop = 
        | P of bool
        | V  of string
        | Dis of Prop * Prop
        | Con of Prop * Prop
        | Neg of Prop


    let rec I p s =
        match p with
        | P(x) -> x
        | V(x) -> Map.find x s
        | Dis(x,y) -> (I x s) || (I y s)
        | Con(x,y) -> (I x s) && (I y s)
        | Neg(x) -> not (I x s)


    (*
    2. A proposition is in negation normal form if the negation operator just appears as applied
    directly to atoms. Write an F# function transforming a proposition into an equivalent proposition
    in negation normal form, using the de Morgan laws:
    ¬(a ∧ b) is equivalent to 
    (¬a) ∨ (¬b) ¬(a ∨ b) is equivalent to (¬a) ∧ (¬b)
    and the law: ¬(¬a) is equivalent to a.
    *)

    let rec toNegationNormal p = 
        match p with
        | Neg(r) -> 
            match r with
            | Neg(s) -> toNegationNormal s
            | Con(s,t) -> Dis(toNegationNormal(Neg(s)),toNegationNormal(Neg(t)))
            | Dis(s,t) -> Con(toNegationNormal(Neg(s)),toNegationNormal(Neg(t)))
            | _ -> Neg(r)
        | Dis(s,t) -> Dis(toNegationNormal s, toNegationNormal t)
        | Con(s,t) -> Con(toNegationNormal s, toNegationNormal t)
        | _ -> p


    (*
    3. A literal is an atom or the negation of an atom and a basic conjunct is a conjunction of literals.
    A proposition is in disjunctive normal form if it is a disjunction of basic conjuncts. Write an F# 
    function which transforms a proposition in negation normal form into an equivalent proposition in
    disjunctive normal form using the distributive laws:
    a∧(b∨c) is equivalent to (a∧b)∨(a∧c) 
    (a∨b)∧c is equivalent to (a∧c)∨(b∧c)
    *)

    let rec isConjunct = function
        | P(_) -> true
        | V(_) -> true
        | Neg(_) -> true
        | Con(s,t) -> if (isConjunct(s) && isConjunct(t)) then true else false
        | _ -> false

    let rec toDisNorm p = 
        match p with
        | Con(s,t) -> 
            match (s,t) with
            | (r,Dis(x,y)) | (Dis(x,y),r) -> 
                let x_norm = toDisNorm(x)
                let y_norm = toDisNorm(y)
                let r_norm = toDisNorm(r)
                Dis(toDisNorm(Con(x_norm,r_norm)),toDisNorm(Con(y_norm,r_norm)))
            | (r,Con(x,y)) | (Con(x,y),r) ->
                let x_norm = toDisNorm(x)
                let y_norm = toDisNorm(y)
                let r_norm = toDisNorm(r)
                if isConjunct(x_norm) && isConjunct(y_norm) && isConjunct(r_norm) then p else
                    toDisNorm(Con(toDisNorm(Con(x_norm,r_norm)), toDisNorm(Con(y_norm,r_norm))))
            | _ -> p
        | Dis(s,t) -> Dis(toDisNorm(s), toDisNorm(t))
        | _ -> p

    (*
    4. We shall use a set-based representation of formulas in disjunctive normal form. 
    Since conjunction is commutative, associative and (a ∧ a) is equivalent to a it is convenient 
    to represent a basic conjunct bc by its set of literals litOf(bc).
    Similarly, we represent a disjunctive normal form formula a: bc1 ∨...∨bcn
    by the set
    dnfToSet(a) = {litOf(bc1), . . . , litOf(bcn)}
    that we will call the dns set of a.
    Write F# declarations for the functions litOf and dnfToSet.
    *)

    let rec litOf = function
        | Con(s, t) -> Set.union (litOf s) (litOf t) 
        | x -> Set.singleton x


    let rec dnfToSet = function
        | Dis(s, t) -> Set.union (dnfToSet s) (dnfToSet t)
        | Con(s, t) -> Set.singleton (Set.union (litOf s) (litOf t))
        | x -> Set.singleton (Set.singleton x)

    (*
    5. A set of literals ls (and the corresponding basic conjunct) is said to be consistent if 
    it does not contain both literals p and ¬p for any atom p. Otherwise, 
    ls (and the corresponding basic conjunct) is said to be inconsistent. 
    An inconsistent basic conjunct yields false regardless of the truth values assigned to atoms. 
    Removing it from a disjunctive normal form formula will therefore not change the meaning of that formula.
    Declare an F# function isConsistent that checks the consistency of a set of literals. 
    Declare an F# function removeInconsistent that removes inconsistent literal sets from a dns set.
    *)

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

    (*
    6. A proposition is satisfiable if it is true for some assignment of truth values to the atoms.
    A formula in disjunctive normal is satisfiable when one (or more) of its basic conjunctions are. 
    Therefore, the set of satisfying assignments of a proposition can be derived from the consistent 
    literal sets of its disjunctive normal form. Declare an F# function toDNFsets that transforms 
    an arbitrary proposition into a dns set with just consistent literal sets.
    Declare a function impl a b for computing the implication a ⇒ b and a function iff a b 
    for computing the bi-implication a ⇔ b.
    Use toDNFsets to determine the satisfying assignments for the following two formulas: 
    • ((¬p)⇒(¬q))⇒(p⇒q)
    • ((¬p)⇒(¬q))⇒(q⇒p) where p and q are atoms.
    *)

    let toDNFsets p =
        p
        |> toNegationNormal
        |> toDisNorm
        |> dnfToSet
        |> removeInconsistent

    let impl (a,b) = 
        Dis(Neg(a),b)

    let iff (a,b) = 
        Con(impl(a, b), impl(b, a))

    (*
    7. In this question you shall solve a Knights and Knaves puzzle, which is a kind of puzzle 
    originating from the logician R. Smullyan. The general theme addresses an island that is 
    inhibited by two kinds of citizens: Knights, who always tell the truth, and knaves, who always 
    tell lies. On the basis of utterances from some inhabitants you must decide what kind they are.
    You may find many Knight and Knave puzzles on the internet. The following is originates 
    from http://www.homeschoolmath.net/reviews/eimacs-logic.php.

    PUZZLE:
    Three of the island's inhabitants – A, B, and C – were talking together. 
    A said, "All of us are knaves." Then B remarked, "Exactly one of us is a knight." 

    What are A, B, and C?
    *)

    let puzzle_part_1 = iff(V "p", Con(Neg(V "p"), Con(Neg(V "q"), Neg(V "r"))))
    let puzzle_part_2 = 
        iff(
            V "q", 
            Dis(
                Dis(
                    Con(
                        V "p", 
                        Con(
                            Neg(V "q"),
                            Neg(V "r"))), 
                    Con(
                        Neg(V "p"), 
                        Con(
                            V "q",
                            Neg(V "r")))),
                Con(
                    Neg(V "p"), 
                    Con(
                        Neg(V "q"),
                        V "r"))))

    let p1 = toDNFsets puzzle_part_1
    let p2 = toDNFsets puzzle_part_2
    let puzzle = Con(puzzle_part_1, puzzle_part_2)
    let result = toDNFsets puzzle
    // The result is: q = true(knight), p = false(knave), r = false(knave) 

    (*
    8. The satisfiability problem for propositional logic is an NP-complete problem, 
    and the above transformation to disjunctive normal form proposition or to a dns set 
    has in the worst case an exponential running time.
    The disjunctive normal form of the following proposition:
    (p1 ∨q1)∧(p2 ∨q2)∧···∧(pn ∨qn) (1)
    will, for example, have 2n basic conjuncts.
    Declare an F# function badProp n that computes the F# representation of (1).
    Compute toDNFsets(badProp n) for a small number of cases and check that the 
    resulting sets indeed have 2n elements.
    *)

    let rec badProp = function
        | n when n=1 -> Dis(V("p" + string n),V("q" + string n))
        | n -> Con(Dis(V("p" + string n),V("q" + string n)), badProp(n-1))



#if DEBUG

    //////////////////////////////
    //           TESTS          //
    //////////////////////////////

    // 1. Helper functions

    let rec compareProps = function
        | (P(s), P(t)) -> if s = t then true else false
        | (Dis(s1, t1), Dis(s2, t2)) -> compareProps (s1,s2) && compareProps (t1,t2)
        | (Con(s1, t1), Con(s2, t2)) -> compareProps (s1,s2) && compareProps (t1,t2)
        | (Neg(s), Neg(t)) -> compareProps (s,t)
        | (_,_) -> false

    let printTestResult testName result =
        let oldColor = System.Console.ForegroundColor
        if result then 
            System.Console.ForegroundColor <- System.ConsoleColor.Green
            printfn "%s OK" testName
        else
            System.Console.ForegroundColor <- System.ConsoleColor.Red
            printfn "%s FAILED" testName
        System.Console.ForegroundColor <- oldColor

    // 2. Preparing data

    let t = P(true)
    let f = P(false)

    let set_1 = Set.ofList [t; Neg(f); f]
    let set_2 = Set.ofList [f; t; Neg(t)]
    let set_3 = Set.ofList [t; Neg(f)]

    let expected_1 = 
        Con(
            Dis(
                Con(
                    Dis(t,Neg(f)),
                    Dis(t,f)),
                t),
            f)

    let expected_2 = 
        Dis(
            Dis(
                Dis(
                    Con(Con(t,t),f),
                    Con(Con(Neg(f),t),f)),
                Dis(
                    Con(Con(t,f),f),
                    Con(Con(Neg(f),f),f))),
            Con(t,f))

    let expected_3 = 
        Set.ofList [
            Set.ofList [f; t]; 
            Set.ofList [f; t; Neg (f)];
            Set.ofList [f; Neg (f)]]

    let expected_4 = 
        Set.ofList [Set.ofList [f; t]]

    let expected_5 = 
        Set.ofList [
            Set.ofList [V "q"]; 
            Set.ofList [V "q"; Neg (V "p")]; 
            Set.ofList [Neg (V "p")]]

    let expected_6 = 
        Set.ofList [
            Set.ofList [V "p"]; 
            Set.ofList [V "q"; Neg (V "p")]; 
            Set.ofList [Neg (V "q")]]

    let formula_1 = 
        impl(
            impl(
                Neg(V "p"), 
                Neg(V "q")),
            impl(
                V "p", 
                V "q"))

    let formula_2 = 
        impl(
            impl(
                Neg(V "p"), 
                Neg(V "q")),
            impl(
                V "q", 
                V "p"))

    // 3. Unit tests

    let toNegationNormal_UnitTest = 
        let result = toNegationNormal(Con(Dis(Neg(Dis(Con(Neg(t),f),Neg(Dis(t,f)))),t),f))
        printTestResult "toNegationNormal_UnitTest" (compareProps (result, expected_1))

    let toDisNorm_UnitTest = 
        let result = toDisNorm(expected_1)
        printTestResult "toDisNorm_UnitTest" (compareProps (result, expected_2))

    let dnfToSet_UnitTest = 
        let result = dnfToSet(expected_2)
        printTestResult "dnfToSet_UnitTest" (result = expected_3)

    let isConsistent_1_UnitTest = 
        let result = isConsistent(set_1)
        printTestResult "isConsistent_1_UnitTest" (result = false)

    let isConsistent_2_UnitTest = 
        let result = isConsistent(set_2)
        printTestResult "isConsistent_2_UnitTest" (result = false)

    let isConsistent_3_UnitTest = 
        let result = isConsistent(set_3)
        printTestResult "isConsistent_3_UnitTest" (result = true)

    let removeInconsistent_UnitTest = 
        let result = removeInconsistent expected_3
        printTestResult "removeInconsistent_UnitTest" (result = expected_4)

    let toDNFsets_1_UnitTest = 
        let result = toDNFsets formula_1
        printTestResult "toDNFsets_1_UnitTest" (result = expected_5)

    let toDNFsets_2_UnitTest = 
        let result = toDNFsets formula_2
        printTestResult "toDNFsets_2_UnitTest" (result = expected_6)

    let badProp_1_UnitTest = 
        let result = toDNFsets(badProp 5)
        printTestResult "badProp_1_UnitTest" (32 = result.Count)

    let badProp_2_UnitTest = 
        let result = toDNFsets(badProp 6)
        printTestResult "badProp_2_UnitTest" (64 = result.Count)
 
#endif