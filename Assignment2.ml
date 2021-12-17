(*
Honor code comes here:

First Name: Jacqueline 
Last Name: Li
BU ID:U81547366

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)

(*
Write a safe_zip_int function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, return None
your method must be tail recursive.

For example,
safe_zip_int [1;2;3;5] [6;7;8;9] = Some [(1,6);(2,7);(3,8);(5,9)]
safe_zip_int [1] [2;4;6;8] = None
safe_zip_int (between 0 1000000) (between 0 1000000) does not stack overflow

Note: The between function is from the previous programming assignment 1. 
You can use the between function from the previous assignment for testing purposes. 
*)


let rec safe_zip_int (ls1: int list) (ls2: int list) : ((int * int) list) option = 
   let rec aux accum ls1 ls2 =
      match ls1,ls2 with
      ([],[]) -> Some(List.rev accum)
      |
      ([],_) -> None
      |
      (_,[]) -> None
      |
      (h1::t1, h2::t2) -> aux ((h1,h2)::accum) t1 t2
      in aux [] ls1 ls2 



(*
Write a function that produces the ith Pell number:
https://en.wikipedia.org/wiki/Pell_number
https://oeis.org/A000129
your function must be tail recursive, and needs to have the correct output up to integer overflow

pell 0 = 0
pell 1 = 1
pell 7 = 169
pell 1000000  does not stack overflow
*)


let rec pell (i: int) : int =
   let rec aux i prev next = 
      match i with
      0 -> prev
      |1 -> next 
      |y -> aux (i-1) next (2*next + prev)
      in aux i 0 1 



(* The nth Tetranacci number T(n) is mathematically defined as follows.
 *
 *      T(0) = 0
 *      T(1) = 1
 *      T(2) = 1
 *      T(3) = 2
 *      T(n) = T(n-1) + T(n-2) + T(n-3) + T(n-4)
 *
 * For more information, you may consult online sources.
 *
 *    https://en.wikipedia.org/wiki/Generalizations_of_Fibonacci_numbers
 *    https://mathworld.wolfram.com/TetranacciNumber.html
 *
 * Write a tail recursive function tetra that computes the nth Tetranacci
 * number efficiently. In particular, large inputs such as (tetra 1000000)
 * should neither cause stackoverflow nor timeout.
*)


let tetra (n : int) : int = 
   let rec aux accum n a b c d =
      if n <= 1 then n
      else if n == 2 then 1
      else if n == 3 then 2
      else 
      match accum with 
      x -> if x <= 3 then a else aux (accum-1) x (a+b+c+d) a b c 
      in aux n n 2 1 1 0



(*
infinite precision natural numbers can be represented as lists of ints between 0 and 9

Write a function that takes an integer and represents it with a list of integers between 0 and 9 where the head 
of the list holds the least signifigant digit and the very last element of the list represents the most significant digit.
If the input is negative return None. We provide you with some use cases:

For example:
toDec 1234 = Some [4; 3; 2; 1]
toDec 0 = Some []
toDec -1234 = None
*)

(* Hint use 
   mod 10
   / 10
*)

let rec toDec (i : int) : int list option = 
   if i == 0 then Some []
   else if i < 0 then None
   else let rec aux accum i =
      if i < 10 then i::accum else aux (i mod 10::accum) (i/10)
      in Some (List.rev(aux [] i))


      
(*
Write a function that sums 2 natrual numbers as represented by a list of integers between 0 and 9 where the head is the least signifigant digit.
Your function should be tail recursive

sum [4; 3; 2; 1] [1;0;1] = [5; 3; 3; 1]
sum [1] [9;9;9] = [0; 0; 0; 1]
sum [] [] = []
sum (nines 1000000) [1] does not stack overflow, when (nines 1000000) provides a list of 1000000 9s
*)

let rec print_list (ls: int list): unit =
   let rec aux ls = match ls with
     | [] -> print_string ""
     | e::[] -> print_int e
     | e::l -> 
       let _ = print_int e 
       in let _ = print_string "; " 
       in aux l
 
   in let _ = print_string "[" 
   in let _ = aux ls
   in         print_string "]" 
 
 

let rec sum (a : int list) (b : int list) : int list =  
   let rec aux accum a b carry =
      match a,b with 
      [],[] -> if carry == 0 then accum else carry::accum
      |
      [],h::t -> if (h+carry)<10 then aux ((h+carry)::accum) [] t 0
                  else if (h+carry)==10 then aux (0::accum) [] t 1
                  else aux ((h+carry mod 10)::accum) [] t ((h+carry)/10)
      |
      h::t,[] -> if (h+carry)<10 then aux ((h+carry)::accum) t [] 0
                  else if (h+carry)==10 then aux (0::accum) t [] 1
                  else aux ((h+carry mod 10)::accum) t [] ((h+carry)/10)
      |
      ha::ta,hb::tb -> if (ha+hb+carry)<10 then aux ((ha+hb+carry)::accum) ta tb 0
                        else if (ha+hb+carry)==10 then aux (0::accum) ta tb 1
                        else aux ((ha+hb+carry mod 10)-10::accum) ta tb ((ha+hb+carry)/10)
      in List.rev(aux [] a b 0)
      
      
(*
Write an infinite precision version of the pel function from before

pell2 0 = []
pell2 1 = [1]
pell2 7 = [9; 6; 1]
pell2 50 = [2; 2; 5; 3; 5; 1; 4; 2; 9; 2; 4; 6; 2; 5; 7; 6; 6; 8; 4]

Hint: You may want to use the sum function from above again inside 
pell2. 

*)


let rec pell2 (i: int) : int list = 
   let rec aux accum i a b =
      if i == 0 then [] 
      else if i == 1 then [1] else
      match accum with 
      m -> if accum <= 1 then a else aux (accum - 1) m (sum (sum a a ) b) a in aux i i [1] [0]


