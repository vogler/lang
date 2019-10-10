Require Import Unicode.Utf8.
Require Import Init.Datatypes.
Require Import Init.Nat.
Require Import Logic.FunctionalExtensionality.
Require Import Program.Equality.


Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Module Type Struct.
  Parameter t: Type.
End Struct.

Module Type Eq <: Struct.
  Include Struct.

  Parameter equal: t -> t -> bool.

  Axiom EqEq: forall x y:t, equal x y = true <-> x = y.

  Lemma EqNeq: forall x y:t, equal x y = false <-> x <> y.
  Proof.  intros x y. split. 
    + intros H J. rewrite J in H. assert (equal y y = true) as G. apply EqEq. reflexivity. rewrite G in H. inversion H.
    + intros H. case_eq (equal x y); intros K. 
      * apply EqEq in K. destruct (H K).
      * reflexivity.
  Qed.

  Lemma EqRefl : forall x, equal x x = true. 
  Proof. intros. apply EqEq. reflexivity. Qed.

  Lemma EqSym : forall x y, equal x y = equal y x. 
  Proof. intros x y. case_eq (equal x y); intros H.
    + apply EqEq in H. rewrite H. symmetry. apply EqEq. reflexivity.
    + apply EqNeq in H. case_eq (equal y x); intro J. 
      * apply EqEq in J. symmetry in J. destruct (H J).
      * reflexivity.
  Qed.
End Eq.


Module Type Dom <: Eq.
  Include Eq.

  Parameter leq : t -> t -> bool.

  Axiom LeqRefl : forall x : t, leq x x = true.

  Axiom LeqTrans : forall x y z : t, leq x y = true -> leq y z = true -> leq x z = true.

  Axiom LeqAntisym : forall x y : t, leq x y = true -> leq y x = true -> x = y.

  Parameter join : t -> t -> t.
  Axiom UBound : forall x y : t, leq x  (join x y) = true.
  Axiom joinSym: forall x y : t, join x y = join y x.

  Axiom lub : forall ub : t -> t -> t, forall x y : t, (leq x (ub x y) = true /\ leq y (ub x y) = true) -> leq (join x y) (ub x y) = true.

  Parameter bot: t.

  Axiom bot_least : forall x : t, leq bot x = true.

  Lemma joinBot: forall x : t, join bot x = x.
  Proof.
    intros. apply LeqAntisym.
    * apply lub with (ub:=fun x y: t => y). auto using LeqRefl,  bot_least.
    * rewrite joinSym. apply UBound.
  Qed.

  Parameter top: t.

  Axiom top_greatest : forall x : t, leq x top = true.

  Lemma joinTop: forall x y : t, join top y = top.
  Proof.
    intros. apply LeqAntisym.
    * apply lub with (ub:=fun x y: t => top). auto using LeqRefl,  top_greatest.
    * apply UBound.
  Qed.

  Parameter widen : t -> t -> t.

  Axiom widen_ub : forall x y : t, leq (join x y) (widen x y) = true.

  Parameter narrow : t -> t -> t.

  Axiom Widen : forall x y : t, leq y x = true -> leq x  (narrow x y) = true /\ leq (narrow x y) y = true.
End Dom.


Module Type PriQ (V : Eq).
  Definition v := V.t.

  Parameter t : Type.

  Parameter empty : t.
  Parameter is_empty : t -> bool.
  Parameter contains : t -> v -> bool.
  Parameter add : v -> t -> t.

  Parameter setPri : v -> nat -> t -> t.
  Parameter getPri : t -> v -> option nat.

  Parameter extract_min : t -> ((v * t) + unit).

  Axiom EmptyPos : forall v: v, is_empty empty = true.
  Axiom EmptyProp : forall (q: t), (forall (v: v), contains q v = false) <-> is_empty q = true.
  Axiom AddZ: forall (q: t) (v: v), contains (add v q) v = true.
  Axiom AddS: forall (q: t) (v1 v2: v), v1<>v2 -> contains (add v1 q) v2 = contains q v2.

  Axiom GetPriE : forall (q: t) (v: v), getPri empty v = None.
  Axiom GetPriZ : forall (q: t) (v: v) (n: nat), getPri (setPri v n q) v = Some n.
  Axiom GetPriS : forall (q: t) (v1 v2: v) (n: nat), (v1 <> v2) -> getPri (setPri v1 n q) v2 = getPri q v2.
  Axiom GetPriAdd : forall (q: t) (v: v), getPri (add v q) v = getPri q v.

  Axiom ExtractE: forall q: t, (extract_min q = inr tt) <-> (forall v: v, contains q v = false).
  Axiom ExtractS: forall (q1 q2: t) (x:v), (extract_min q1 = inl (x,q2)) ->
      (contains q1 x = true) /\
      (contains q2 x = false) /\
      (forall x': v, (x <> x') -> contains q1 x' = contains q2 x').

  Definition optLeq (x: option nat) (y: option nat) : Prop :=
    match x with
         | Some n =>
                match y with
                     | Some n0 => n <= n0
                     | None => True
                end
         | None => True
   end.

  Axiom ExtractP: forall (q1 q2: t) (x:v), (extract_min q1 = inl (x,q2)) ->
      (forall x': v, contains q2 x' = true -> optLeq (getPri q2 x') (getPri q1 x)).

End PriQ.


Module Type Monad.
  Parameter m : Type -> Type.

  Parameter ret : forall a: Type, a -> m a.
  Parameter bind : forall a b: Type, m a -> (a -> m b) -> m b.
  Notation "x >>= f" := (bind x f) (at level 30).

  Definition bind_ (a b: Type) (x: m a) (y: m b) : m b := bind x (fun _ => y).
  Notation "x >> y" := (bind x y) (at level 30).

  Axiom left_id: forall (a b:Type) (x: a) (f: a -> m b),  ret x >>= f  =  f x.
  Axiom right_id: forall (a:Type) (m: m a),  m >>= @ret a  =  m.
  Axiom assoc: forall (a b c: Type) (x: m a) (f: a -> m b) (g: b -> m c), (x >>= f) >>= g  =  x >>= (fun x => f x >>= g).
End Monad.

Module StateM (S: Struct) <: Monad.
  Definition s := S.t.

  Definition m := fun (r: Type) => s -> r * s.

  Definition ret (a: Type) (x: a) (y: s) := (x, y).

  Definition bind (a b: Type) (x: m a) (f: a -> m b) (y: s) := 
    match x y with (z,w) => f z w end.
  Notation "x >>= f" := (bind x f) (at level 30).

  Definition bind_ (a b: Type) (x: m a) (y: m b) : m b := bind x (fun _ => y).
  Notation "x >> y" := (bind x y) (at level 30).

  Lemma left_id: forall (a b:Type) (x: a) (f: a -> m b),  ret x >>= f  =  f x.
  Proof. trivial. Qed.

  Lemma right_id: forall (a:Type) (x: m a),  (x >>= @ret a)  =  x.
  Proof. intros. apply functional_extensionality. intro y. unfold bind, ret. destruct (x y). trivial. Qed.

  Lemma assoc: forall (a b c: Type) (x: m a) (f: a -> m b) (g: b -> m c), x >>= f >>= g  =  x >>= (fun x => f x >>= g).
  Proof. 
    intros a b c x f g. apply functional_extensionality. intros y.
    unfold bind, ret. destruct (x y). trivial.
  Qed.
End StateM.


Module OptionM <: Monad.
  Definition m := option.

  Definition ret (a: Type) (x: a) := Some x.

  Definition bind (a b: Type) (x: option a) (f: a -> option b) :=
    match x with None => None | Some y => f y end.
  Notation "x >>= f" := (bind x f) (at level 30).

  Definition bind_ (a b: Type) (x: m a) (y: m b) : m b := bind x (fun _ => y).
  Notation "x >> y" := (bind x y) (at level 30).

  Lemma left_id: forall (a b:Type) (x: a) (f: a -> m b),  ret x >>= f  =  f x.
  Proof. trivial. Qed.

  Lemma right_id: forall (a:Type) (x: m a),  (x >>= @ret a)  =  x.
  Proof. intros. unfold bind, ret. destruct x; trivial. Qed.

  Lemma assoc: forall (a b c: Type) (x: m a) (f: a -> m b) (g: b -> m c), (x >>= f) >>= g  =  x >>= (fun x => f x >>= g).
  Proof. 
    intros a b c x f g. unfold bind, ret. destruct x; trivial. 
  Qed.
End OptionM.

Module StateOptionM (S: Struct) <: Monad.
  Module Sm := StateM (S).
  Definition s := S.t.

  Definition m (t:Type) := Sm.m (OptionM.m t).

  Definition ret (a: Type) (x: a) : m a := fun y: s => (Some x, y).

  Definition bind (a b: Type) (x: m a) (f: a -> m b) : m b := fun y: s =>
    match x y with 
      | (None, s') => (None, s') 
      | (Some z, s') => f z s' 
    end.

  Definition bind_ (a b: Type) (x: m a) (y: m b) : m b := bind x (fun _ => y).

  Notation "x >> y" := (bind_ x y) (at level 30, right associativity).
  Notation "x >>= f" := (bind x f) (at level 30, right associativity).

  Definition fail (a: Type) : m a := Sm.ret None.

  Definition get: m s := fun y: s => (Some y, y).
  Definition set (x: s):  m unit := fun _: s => (Some tt, x).
  Definition modify (f: s -> s):  m unit := fun x: s => (Some tt, f x).

  Definition lift (a: Type) (x: option a) := fun y: s => (x, y).

  Lemma left_id: forall (a b:Type) (x: a) (f: a -> m b),  ret x >>= f  =  f x.
  Proof. trivial. Qed.

  Lemma right_id: forall (a:Type) (x: m a),  (x >>= @ret a)  =  x.
  Proof. intros. apply functional_extensionality. intro y. unfold bind, ret. destruct (x y), m0; trivial. Qed.

  Lemma assoc: forall (a b c: Type) (x: m a) (f: a -> m b) (g: b -> m c), (x >>= f) >>= g  =  x >>= (fun x => f x >>= g).
  Proof. 
    intros a b c x f g. apply functional_extensionality. intro y. unfold bind, ret. destruct (x y), m0; [destruct (f a0 s0), m0; auto|auto].
  Qed.
End StateOptionM.

Module Type set (El: Eq).
  Definition el := El.t.
  Parameter set : Type.

  Parameter empty : set.
  Parameter is_empty : set -> bool.
  Parameter contains : set -> el -> bool.
  Parameter add: el -> set -> set.
  Parameter remove: el -> set -> set.
  Parameter extract: set -> (el * set) + unit.
  Parameter fold: forall a: Type, set -> a -> (el -> a -> a) -> a.

  Parameter SetInv : set -> Type.

  Axiom emptySet : SetInv empty.
  Axiom addSet : forall (s: set) (e: el), SetInv s -> SetInv (add e s).
  Axiom removeSet : forall (s: set) (e: el), SetInv s -> SetInv (remove e s).
  Axiom extractSet : forall (s1 s2: set) (e: el), SetInv s1 -> extract s1 = inl (e, s2) -> SetInv s2.

  Axiom EmptyZ: is_empty empty = true.
  Axiom EmptyS: forall s: set, is_empty s = true <-> forall e: el, contains s e = false.

  Axiom AddZ: forall (s: set) (e:el), contains (add e s) e = true.
  Axiom AddS: forall (s: set) (e1 e2:el), El.equal e1 e2 = false -> contains (add e1 s) e2 = contains s e2.

  Axiom RemoveZ: forall (s: set) (e:el), SetInv s -> contains (remove e s) e = false.
  Axiom RemoveS:  forall (s: set) (e1 e2:el), SetInv s -> e1<>e2 -> contains (remove e1 s) e2 = contains s e2.

  Axiom ExtractE: forall (s: set), is_empty s = true  <->  extract s = inr tt.
  Axiom ExtractNE: forall (s1: set), is_empty s1 = false  <->  exists (e:el), extract s1 = inl (e,remove e s1).

  Axiom Fold: forall (s: set) (a: Type) (e: a) (f: el -> a -> a), 
      if is_empty s then fold s e f = e else exists x: el, contains s x = true ->  fold s e f = fold (remove x s) (f x e) f.
End set.


Module ListSet (El : Eq) : set (El).
  Definition el := El.t.

  Definition set := list el.

  Definition empty := @nil el.
  Definition is_empty (xs: set) :=
    match xs with nil => true | cons x ys => false end.
  Fixpoint contains (xs: set) (x: el) :=
    match xs with
      | nil => false
      | cons y ys => if El.equal x y then true else contains ys x 
    end.
  Definition add (e: el) (xs: set) := if contains xs e then xs else cons e xs.

  Fixpoint remove (e: el) (xs: set) :=
    match xs with
      | cons e' xs' => if El.equal e e' then xs' else cons e' (remove e xs')
      | nil => nil
    end.

  Fixpoint fold (a: Type) (s: set) (e: a) (f: el -> a -> a): a := 
    match s with
      | nil => e
      | cons x xs => fold xs (f x e) f
    end.

  Definition extract (xs: set) :=
    match xs with nil => inr tt | cons y ys =>  inl (y, remove y xs) end.

  Inductive InList (t:Type) : t -> list t -> Prop :=
    | here: forall (x: t) (xs: list t), InList x (cons x xs)
    | there: forall (x y: t) (xs: list t), InList x xs -> InList x (cons y xs).

  Lemma containsInList : forall (x: el) (xs: set), contains xs x = true <-> InList x xs.
  Proof. intros. split.
    * intros H. induction xs. 
      + unfold contains in H. inversion H. 
      + unfold contains in H. case_eq (El.equal x a); intros G; fold contains in H.
          - apply El.EqEq in G. rewrite G. apply here.
          -  rewrite G in H. apply there, IHxs, H. 
    * intros H. induction xs.
      + inversion H.
      + unfold contains. inversion H; fold contains.
          -  rewrite El.EqRefl. reflexivity.
          - destruct (El.equal x a); [trivial|]. apply IHxs, H2.
  Qed.

  Lemma containsNotInList : forall (x: el) (xs: set), contains xs x = false <-> (InList x xs -> False).
  Proof.  intros. split; intros.
    + apply containsInList in H0. rewrite H0 in H. inversion H.
    + case_eq (contains xs x); intros E. 
        * apply containsInList in E. destruct (H E).
        * reflexivity.
  Qed.


  Inductive Unique (t:Type) : list t -> Prop :=
    | emptyUnique: Unique nil
    | consUnique: forall (x: t) (xs: list t), (InList x xs -> False) -> Unique xs -> Unique (cons x xs).

  Definition SetInv:= @Unique el .

  Lemma emptySet : SetInv empty.
  Proof. apply emptyUnique. Qed.

  Lemma addSet : forall (s: set) (e: el), SetInv s -> SetInv (add e s).
  Proof. intros. unfold add. case_eq (contains s e); intros G; [apply H|]. apply consUnique. 
  + intros J. apply containsInList in J. rewrite G in J. inversion J.
  + apply H.
  Qed.


  Lemma remDecr : forall (x: el) (xs: set), (InList x xs -> False) -> forall e, InList x (remove e xs) -> False.
  Proof. intros x xs H e Q. 
  induction xs. 
    * inversion Q.
    * unfold remove in Q. destruct (El.equal e a); fold remove in Q.
      + apply H, there, Q.
      + apply IHxs.
        ++ intro W. apply H, there, W.
        ++ clear IHxs. inversion Q.
            *** destruct H. rewrite H2. apply here.
            *** apply H2.
  Qed.

  Lemma removeSet : forall (s: set) (e: el), SetInv s -> SetInv (remove e s).
  Proof. intros. induction H.
    + apply emptyUnique.
    + unfold remove. destruct (El.equal e x); [| fold remove]. 
       * auto.
       * apply consUnique. 
            ++ apply remDecr, H.
            ++ auto.
  Qed.

  Lemma extractSet : forall (s1 s2: set) (e: el), SetInv s1 -> extract s1 = inl (e, s2) -> SetInv s2.
  Proof. intros s1 s2 e H1 H2. induction s1.
    + inversion H2.
    + unfold extract in H2. inversion H2. rewrite (El.EqRefl e). inversion H1. auto. 
  Qed.


  Lemma EmptyZ: is_empty empty = true.
  Proof. unfold is_empty, empty. trivial. Qed.


  Lemma EmptyS: forall s: set, is_empty s = true <-> forall e: el, (contains s e = false).
  Proof. intros. induction s; [|split]. 
    * firstorder.
    * intros H. inversion H.
    * intros H. specialize (H a). unfold contains in H. rewrite El.EqRefl in H. inversion H.
  Qed.

  Lemma AddZ: forall (s: set) (e:el), contains (add e s) e = true.
  Proof. intros s e. unfold add. case_eq (contains s e); intros H. * exact H. * unfold contains. rewrite El.EqRefl. reflexivity.
  Qed.

  Lemma AddS: forall (s: set) (e1 e2:el), El.equal e1 e2 = false -> contains (add e1 s) e2 = contains s e2.
  Proof. intros s e1 e2 H. unfold add. destruct (contains s e1).
    * reflexivity.
    * unfold contains at 1. rewrite El.EqSym in H. rewrite H. fold contains. trivial.
  Qed.

  Lemma RemoveZ: forall (s: set) (e:el), SetInv s -> contains (remove e s) e = false.
  Proof. intros. induction s; [trivial|]. unfold remove. case_eq (El.equal e a); intro E ; [|fold remove].
  + apply El.EqEq in E. destruct E.  apply containsNotInList. intros Q. inversion H. auto.
  + unfold contains. destruct (El.equal e a); fold contains.
    ++ inversion E.
    ++ apply IHs.  inversion H. auto.
  Qed.

  Lemma RemoveS:  forall (s: set) (e1 e2:el), SetInv s -> e1<>e2 -> contains (remove e1 s) e2 = contains s e2.
  Proof. intros. induction s.
    + auto.
    + unfold remove; case_eq (El.equal e1 a); intro E; fold remove.
        ++ apply El.EqEq in E. destruct E. unfold contains at 2. apply El.EqNeq in H0. rewrite El.EqSym in H0. rewrite H0. fold contains. auto.
        ++ unfold contains. destruct (El.equal e2 a); fold contains.
            +++ reflexivity.
            +++  apply IHs. inversion H. auto.
  Qed.

  Lemma ExtractE: forall (s: set), is_empty s = true  <->  extract s = inr tt.
  Proof. intros s. split.
    + intros H. unfold extract. destruct s. * reflexivity. * unfold is_empty in H. inversion H. 
    + intros H. unfold extract in H. unfold is_empty. destruct s. * reflexivity. * inversion H.
  Qed.

  Lemma ExtractNE: forall (s: set), is_empty s = false  <->  exists e:el, extract s = inl (e, remove e s).
  Proof. intros s. split.
    + intros. unfold is_empty in H. destruct s. * inversion H. * exists e. auto.
    + intros. unfold extract in H. destruct H. destruct s. * inversion H. * auto.
  Qed.

  Lemma Fold: forall (s: set) (a: Type) (e: a) (f: el -> a -> a), 
      if is_empty s then fold s e f = e else exists x: el, contains s x = true ->  fold s e f = fold (remove x s) (f x e) f.
  Proof. intros. destruct s.
    + unfold is_empty, fold. reflexivity.
    + unfold is_empty. exists e0. intros. unfold fold; fold fold. unfold remove; rewrite (El.EqRefl e0). reflexivity.
  Qed.

End ListSet.

Module Type Map (El: Eq).
  Definition key := El.t.

  Parameter map : Type -> Type.

  Parameter empty: forall t: Type, map t.
  Parameter add: forall t: Type, key -> t ->  map t -> map t.
  Parameter remove: forall t: Type, key ->  map t -> map t.
  Parameter find: forall t: Type, key -> map t -> option t.

  Parameter MapInv : forall t, map t -> Prop.

  Axiom EmptyInv: forall t, MapInv (empty t).
  Axiom AddInv: forall (t:Type) (k: key) (x: t) (m: map t), MapInv m -> MapInv (add k x m).
  Axiom RemoveInv: forall (t:Type) (k: key) (m: map t), MapInv m -> MapInv (remove k m).

  Axiom EmptyZ: forall (t: Type) (k: key), find k (empty t) = None.
  Axiom AddZ: forall (t: Type) (k: key) (x: t) (m: map t), find k (add k x m) = Some x.
  Axiom AddS: forall (t: Type) (k1 k2: key) (x: t) (m: map t), MapInv m -> k1 <> k2 -> find k2 (add k1 x m) = find k2 m.
  Axiom RemoveZ: forall (t: Type) (k: key) (m: map t), MapInv m -> find k (remove k m) = None.
  Axiom RemoveS: forall (t: Type) (k1 k2: key) (m: map t), MapInv m -> k1 <> k2 -> find k1 (remove k2 m) = find k1 m.

End Map.

Module AssocMap (Key : Eq) : Map (Key).
  Definition key := Key.t.

  Definition map (t: Type) := list (key * t).

  Definition empty (t:Type) : map t := nil.

  Fixpoint remove (t: Type) (k1: key) (m: map t) := 
    match m with
      | nil => nil
      | cons (k2, v) xs => if Key.equal k1 k2 then xs else cons (k2,v) (remove k1 xs)
    end.

  Definition add (t: Type) (k: key) (x: t) (m: map t) := 
    cons (k, x) (remove k m).

  Fixpoint find (t: Type) (k1: key) (m: map t) := 
    match m with
      | nil => None
      | cons (k2,v) xs => if Key.equal k1 k2 then Some v else find k1 xs
    end.

  Fixpoint MapInv (t: Type) (m: map t):= 
    match m with
      | nil => True
      | cons (k, _) xs => MapInv xs /\ find k xs = None
    end.

  Lemma EmptyInv: forall t, MapInv (empty t).
  Proof. intros. unfold empty, MapInv. trivial. Qed.

  Lemma RemoveDecr: forall (t: Type) (k1 k2:key) (m: map t), find k1 m = None -> find k1 (remove k2 m) = None.
  Proof. intros. induction m; [auto|]. unfold remove; fold remove. destruct a. destruct (Key.equal k2 k).
    + unfold find in H; fold find in H. destruct (Key.equal k1 k); [inversion H|auto].
    + unfold find; unfold find in H; destruct (Key.equal k1 k); fold find; fold find in H.
      ++ inversion H.
      ++ apply IHm, H.
  Qed.

  Lemma RemoveInv: forall (t:Type) (k: key) (m: map t), MapInv m -> MapInv (remove k m).
  Proof. intros. induction m; [unfold remove; trivial |].
    unfold remove; fold remove. destruct a. destruct (Key.equal k k0).
      +  inversion H. trivial.
      + unfold MapInv; fold MapInv. split.
          * apply IHm. inversion H. trivial.
          * apply RemoveDecr. inversion H. auto.
  Qed.

   Lemma RemoveZ: forall (t:Type) (k: key) (m: map t), MapInv m -> find k (remove k m) = None.
    Proof. intros. induction m; [unfold remove; unfold find; auto|].
    unfold remove. destruct a. case_eq (Key.equal k k0); intros E.
    + unfold MapInv in H; fold MapInv in H. apply Key.EqEq in E. rewrite E. inversion H. auto.
    + fold remove. unfold find. rewrite E. fold find. apply IHm. inversion H. auto.
    Qed.

  Lemma AddInv: forall (t:Type) (k: key) (x: t) (m: map t), MapInv m -> MapInv (add k x m).
  Proof. intros. destruct m. unfold add.
    + unfold remove. split; auto.
    + unfold add. unfold MapInv; fold MapInv. split.
      ++ apply RemoveInv. auto.
      ++ apply RemoveZ. auto. 
  Qed.


  Lemma EmptyZ: forall (t: Type) (k: key), find k (empty t) = None.
  Proof. intros. unfold empty, find. trivial. Qed.

  Lemma AddZ: forall (t: Type) (k: key) (x: t) (m: map t), find k (add k x m) = Some x.
  Proof.  intros. unfold add, find. rewrite (Key.EqRefl k). reflexivity. Qed.

  Lemma RemoveS: forall (t: Type) (k1 k2: key) (m: map t), MapInv m -> k1 <> k2 -> find k1 (remove k2 m) = find k1 m.
  Proof. intros. induction m.
    + unfold remove, find. reflexivity.
    + unfold remove, find. fold remove. fold find. destruct a. case_eq (Key.equal k2 k); case_eq (Key.equal k1 k); intros E1 E2.
      * exfalso. apply H0. apply Key.EqEq in E1. apply Key.EqEq in E2. destruct E1, E2. trivial.
      * trivial.
      * unfold find. rewrite E1. trivial.
      * unfold find. rewrite E1. fold find. apply IHm. inversion H. trivial.
  Qed.

  Lemma AddS: forall (t: Type) (k1 k2: key) (x: t) (m: map t), MapInv m -> k1 <> k2 -> find k2 (add k1 x m) = find k2 m.
  Proof. intros. unfold add. unfold find; fold find. apply Key.EqNeq in H0. rewrite Key.EqSym in H0. rewrite H0.
  apply RemoveS; [auto|]. apply Key.EqNeq. auto.  
  Qed.


End AssocMap.

Module Type Constraints (D : Dom) (V: Eq).
  Parameter f : forall (m: Type -> Type), (forall a:Type, a -> m a) -> (forall a b:Type, m a -> (a -> m b) -> m b) -> (V.t -> m D.t) -> V.t -> m D.t.
End Constraints.

Module Solver (Import D_ : Dom) (Import V_: Eq) (Import F_: Constraints D_ V_) (Import Q : PriQ V_).

Definition d := D_.t.
Definition v := V_.t.

Module Vset := ListSet (V_).
Module Vmap := AssocMap (V_).

Definition dom := Vset.set.
Definition next_prio := nat.
Definition sigma := Vmap.map d.
Definition point := Vset.set.
Definition infl := Vmap.map Vset.set.

Definition state : Type := (dom * next_prio * sigma * Q.t * point * infl).

Definition getDom (s: state) : dom := let '(d,n,s,q,p,i) := s in d .
Definition getNextPrio(s: state) : next_prio := let '(d,n,s,q,p,i) := s in n .
Definition getSigma(s: state) : sigma := let '(d,n,s,q,p,i) := s in s .
Definition getQ(s: state) : Q.t := let '(d,n,s,q,p,i) := s in q .
Definition getPoint(s: state) : point := let '(d,n,s,q,p,i) := s in p .
Definition getInf(s: state) : infl := let '(d,n,s,q,p,i) := s in i .


Module State <: Struct.
  Definition t : Type := state.
End State.

Module SOM := StateOptionM State.
Import SOM.

Fixpoint solve (c: nat) (y: v) {struct c}: m unit :=
  match c with
    | 0 => fail unit
    | S c' => 
       get >>= fun s: state => 
       let '(dom, prio, sigma, q, point, infl) := s in
       if Vset.contains dom y then 
          ret tt
        else
          set (Vset.add y dom, S prio, sigma, q, point, infl) >>
          do_var c' false y >>= fun b': bool =>
          get >>= fun s': state =>
          let '(dom', prio', sigma', q', point', infl') := s' in
          lift (getPri q y) >>= fun p: nat =>
          iterate c' b' p
  end

with do_var (c: nat) (b: bool) (y: v) {struct c} : m bool := 
    match c with
      | 0 => fail bool
      | S c' => 
        get >>= fun s: state => 
        let '(dom, prio, sigma, q, point, infl) := s in

        let isp := Vset.contains point y in
        set (dom, prio, sigma, q, Vset.remove y point, infl) >>
        @f m ret bind (eval c' y) y  >>= fun tmp: d =>

        get >>= fun s': state => 
        let '(dom', prio', sigma', q', point', infl') := s' in

        lift (Vmap.find y sigma') >>= fun sigmay: d =>
        let ' (tmp', b') :=
          if isp then
            if b then
              (narrow tmp sigmay, b) 
            else if leq tmp sigmay then
              (narrow tmp sigmay, true)
            else
              (widen sigmay tmp, b)
          else
            (tmp, b)
        in
        if D_.equal sigmay tmp' then
          ret true
        else (
          lift (Vmap.find y infl') >>= fun infly: Vset.set =>
          let q'' := Vset.fold infly q' Q.add in
          set (dom', prio', Vmap.add y tmp sigma', q'', point', Vmap.add y Vset.empty infl') >>
          ret b'
      )
    end

with eval (c: nat) (y: v) (z: v): m d := 
    match c with
      | 0 => fail d
      | S c' => 
          solve c' z >>
          get >>= fun s: state =>
          let '(dom, prio, sigma, q, point, infl) := s in
          lift (getPri q z) >>= fun pz: nat =>
          lift (getPri q y) >>= fun py: nat =>
          let point' :=  if (py <=? pz) then Vset.add z point else point in
          lift (Vmap.find z infl) >>= fun inflz: Vset.set =>
          let infl' := Vmap.add z (Vset.add y inflz) infl  in
          set (dom, prio, sigma, q, point', infl') >>
          lift (Vmap.find z sigma) >>= fun sigmaz: d =>
          ret sigmaz
  end

with iterate (c: nat) (b: bool) (n: nat) {struct c} : m unit := 
  match c with
    | 0 => fail unit
    | S c' => 
      get >>= fun s: state =>
      let '(dom, prio, sigma, q, point, infl) := s in
      if (is_empty q) then 
        match (extract_min q) with
            | inl (y, q') => 
                lift (getPri q y) >>= fun n': nat =>
                if (n' <=? n) then 
                  do_var c' b y >>= fun b' =>
                  if (negb (xorb b b') && (n' <? n))%bool then
                    iterate c' b' n' >>
                    iterate c' b n
                  else
                    iterate c' b' n
                else
                  fail unit
            | inr tt => fail unit
        end
      else
        ret tt
    end.



Definition Hoare (a: Type) (pre: state -> Prop) (x: m a) (post: state -> Prop): Prop :=
    forall s: state, pre s -> post (snd (x s)).

Definition PriOrDom (s: state) := forall x: v, ((exists n:nat, Q.getPri (getQ s) x = Some n) \/ (Vset.contains (getDom s) x = true)).

Lemma PriOrDomIter: (forall (c:nat) (b: bool) (y: v), Hoare PriOrDom (do_var c b y) PriOrDom) -> forall (c:nat)  (b: bool) (n: nat), Hoare PriOrDom (iterate c b n) PriOrDom.
Proof.
  intros H c b n s pre x. unfold PriOrDom in pre. assert (pre x) as HH.
Qed.

Lemma PriWrittenIn: (forall (c:nat) (b: bool) (y: v), Hoare PriOk (do_var c b y) PriOk) -> forall (c:nat)  (b: bool) (n: nat), Hoare PriOk (iterate c b n) PriOk.

Definition PriOk (s: state) := forall x: v, Q.contains (getQ s) x = true -> exists p: nat, Q.getPri (getQ s) x = Some p.

Lemma PriOkIn: (forall (c:nat) (b: bool) (y: v), Hoare PriOk (do_var c b y) PriOk) -> forall (c:nat)  (b: bool) (n: nat), Hoare PriOk (iterate c b n) PriOk.
Proof. intros H c b n s Pre x xinq. destruct s, p, p. destruct (getPri t0).
    + exact x.
    + 
    +
  
Qed.
