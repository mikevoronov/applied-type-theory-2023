(*дата сдачи - 20 декабря*)

Section List.

Inductive List (A: Type): Type :=
| Nil: List A
| Cons: A -> List A -> List A.

Arguments Nil {A}.
Arguments Cons {A}.

Fixpoint append {A : Type} (left right: List A): List A :=
    match left with
    | Nil => right
    | Cons head tail => Cons head (append tail right)
    end.


(* 
    1. Докажите следующие теоремы для списка и вектора
*)

(* 
    (1 балл)
*)

Theorem append_nil_l {A: Type}:
    forall (u: List A), append Nil u = u.
Proof.
    Admitted.

(* 
    (3 балла)
*)
Theorem append_nil_r {A: Type}:
    forall (u: List A), append u Nil = u.
Proof.
    Admitted.

(*
    (3 балла)
*)
Theorem append_assoc {A: Type}:
    forall (u v w: List A),
    append (append u v) w = append u (append v w).
Proof.
    Admitted.

Fixpoint list_length {A: Type} (l: List A): nat :=
    match l with
    | Nil => O
    | Cons head tail => S (list_length tail)
    end.

(*
    (3 балла)
*)

Theorem append_length {A: Type}: forall (l1 l2: List A),
    list_length (append l1 l2) = (list_length l1) + (list_length l2).
Proof.
    Admitted.


Fixpoint rev {A: Type} (l: List A) : List A :=
    match l with
    | Nil => Nil
    | Cons head tail => append  (rev tail) (Cons head Nil)
    end.

(*
    (4 балла)
*)

Theorem rev_length_the_same {A: Type}: forall (l: List A),
    list_length (rev l) = list_length l.
Proof.
    Admitted.
    
(*
    (3 балла)
*)
Theorem rev_involutive {A: Type}: forall (l: List A),
    rev (rev l) = l.
Proof.
    Admitted.

End List.

Section Vector.

Inductive Vector (A: Type): nat -> Type :=
 | VNil: Vector A 0
 | VCons n: A -> Vector A n -> Vector A (S n).

(*
    (3 балла)
*)
Definition cast_vector {A: Type} {m n:nat}:
    Vector A m -> m = n -> Vector A n.
Proof.
    Admitted.


End Vector.

(*
    2. (3 балла) Сколько обитателей у следующего типа
    (напишите число с обоснованием без непосредственного доказательства)
*)

Inductive t : Type :=
  | Ctor1 : t -> t
  | Ctor2 : t -> bool -> t.

(*
    3. (4 балла) Докажите, что квадрат суммы натуральных чисел равен сумме кубов:
    ((sum i from 0 to n) i )^2 = (sum i from 0 to n) i^3
*)


(*
    4. (5 баллов)* Определите J-элиминатор
*)

Definition J :
  forall (A : Type) (P : forall (x y : A), x = y -> Prop),
    (forall x : A, P x x eq_refl) ->
    forall x y (p : x = y), P x y p
:= 