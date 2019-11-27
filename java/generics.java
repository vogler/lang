import java.util.function.*;

class Generics { // class with main needs to come first to be able to use `java generics.java`
  public static void takeA(A x){ }
  public static void takeIA(IA x){ }

  public static void main(String[] args) {
    takeA(new A());
    // for arguments we can always supply a subtype (either via `extends` (single) or `implements` (multiple))
    takeA(new B());
    // takeIA(new A()); // nominal typing: does not work without `implements IA`
    takeIA(new C());

    // so why do we need `<? extends T>`, shouldn't it be the default for everything?
    // answer: the automatic subtyping above only works for method arguments, not for generic classes!
    // why? because they don't want e.g. `List<String> ls` to automatically be a subtype of `List<Object> lo`
    // why? because then `ls.add(new Object())` should be allowed (since you could supply a subclass `List<String>` as an argument where `List<Object>` is expected and call its methods). This could be allowed if we only had immutable values (inside the function you would only see `List<Object>` and you could add `Object` or any subtype), but since the function can add `Object` to a referenced `List<String>` that might be used elsewhere, it can't be allowed.
    // I.e. for generic arguments we have implicit subtyping, but for type variables going through classes it needs to be explicitly allowed because it could be unsafe for references/mutable state.
    // E.g. for Function<T,R>.andThen it specifies subtyping in both directions:
    // <V> Function<T,V> andThen(Function<? super R, ? extends V> after)
    // f3 = f1.andThen(f2)
    // f1: T -> R, f2: >R -> <V, f3: T -> V
    // i.e. f2 needs to accept R or any supertype and return V or any subtype
    Function<Integer, Integer> f1 = x -> x+1;
    Function<Integer, Integer> f2 = x -> x*2;
    System.out.println(f1.andThen(f2).apply(1)); // 4
    Function<Object, B> f2b = x -> new C(); f1.andThen(f2b); // Object > Integer, C < B
  }
}

interface IA { int a(); }
interface IB { int b(); }

class A {
  public int a() { return 1; }
}
class B extends A {
  public int b() { return 2; }
}
class C extends B implements IA, IB {}
