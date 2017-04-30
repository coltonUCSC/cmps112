(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let rec compare' list1 list2 = match (list1, list2) with
        | [], []    -> 0 (* both are empty, lists are of equal length *)
        | list1, [] -> 1 (* left operand must be larger *)
        | [], list2 -> -1 (* right operand must be larger *)
        | car1::cdr1, car2::cdr2 -> 
          let value = compare' cdr1 cdr2 in
          if (value = 0 && car1 != car2) then
              if (car1 > car2) then 1
              else -1
          else value

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            let top = (car1 - carry) + (if car1 < car2 then 10 else 0)
            in let diff = top - car2
            in diff :: sub' cdr1 cdr2 (if car1 < car2 then 1 else 0)


    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            let sum = car1 + car2 + carry
            in  (printf "c1: %d c2: %d carry: %d sum: %d\n%!" car1 car2 carry sum); sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec mul'' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> []
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list2 [carry] 0 
        | [], list2, carry   -> []
        | car1::cdr1, car2::cdr2, carry ->
            let prod = (car1 * car2) + carry
            in  (printf "c1: %d c2: %d carry: %d prod: %d\n%!" car1 car2 carry prod); prod mod radix :: mul'' [car1] cdr2 (prod / radix)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let cmp = (compare' value1 value2) in
        (printf "cmp: %d\n%!" cmp);
        if neg1 = neg2 then 
        begin
            if cmp = -1 then
            begin
                let sign = (if neg1 = Pos then Neg else Pos) 
                in Bigint (sign, sub' value2 value1 0)
            end
            else
                Bigint (neg1, sub' value1 value2 0)
        end
        else
            Bigint (neg1, add' value1 value2 0)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let cmp = (compare' value1 value2) in
        (printf "cmp: %d\n%!" cmp);
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else
            begin
                 if cmp = -1 then
                    Bigint (neg2, sub' value2 value1 0)
                 else
                    Bigint (neg1, sub' value1 value2 0)
            end 

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        Bigint (Pos, mul'' value1 value2 0)

    let div = add

    let rem = add

    let pow = add

end
