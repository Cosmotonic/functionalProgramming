
import java.util.List;
import java.util.ArrayList;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.BiFunction;
import java.util.Collections;

class Pair<A, B> {
  public final A fst;
  public final B snd;

  public Pair(A fst, B snd) {
    this.fst = fst;
    this.snd = snd;
  }

  public String toString() {
    return String.format("(%s,%s)",fst,snd);
  }
}

class Main {

  public static void printInt(int n) {
    System.out.printf("%d \n", n);
  }

  public static void printDouble(double n) {
    System.out.printf("%.2f \n", n);
  }

  public static void printString(String s) {
    System.out.printf("%s \n", s);
  }

  public static void printBool(boolean b) {
    System.out.printf("%b \n", b);
  }

  public static <T> void printList(List<T> xs) {
    System.out.printf("[ ");
    for (T x : xs) {
      System.out.printf("%s ", x);
    }
    System.out.printf("]\n");    
  }
  
  public static <T, U> List<U> map (Function<T, U> f, List<T> xs) {
    List<U> res = new ArrayList<>();
    for(T x : xs) {
      res.add(f.apply(x));
    }
    return res;
  }

  public static int square (int x) {
    return x * x;
  }

  public static int add5 (int x) {
    return x + 5;
  }

  // :: makes square a reference to method square in class Main.
  public static List<Integer> squareList(List<Integer> xs) {
    return map(Main::square, xs);
  }

  // Alternative to ::, create a lambda.
  public static List<Integer> add5List(List<Integer> xs) {
    return map(x -> Main.add5(x), xs);
  }

  public static List<Boolean> posList(List<Integer> xs) {
    List<Boolean> res = new ArrayList<>();
    for (Integer x : xs) {
      res.add(x < 0 ? false : true);
    }
    return res;
  }

  public static List<Integer> addElems(List<Pair<Integer,Integer>> xs) {
    List<Integer> res = new ArrayList<>();
    for (Pair<Integer,Integer> x : xs) {
      res.add(x.fst + x.snd);
    }
    return res;
  }

  public static <T> boolean isMember(T y, List<T> xs) {
    return xs.stream().anyMatch(x -> x.equals(y));
  }

  public static <T> boolean forAll(Predicate<T> p, List<T> xs) {
    return xs.stream().allMatch(p);    
  }

  public static <T> List<T> filter(Predicate<T> p, List<T> xs) {
    return xs.stream().filter(p).toList();    
  }

  // Impleted as a recursive method
  public static <T, U> U foldBack(BiFunction<T, U, U> f, List<T> xs, U acc) {
    // Stop recursion
    if (xs.isEmpty()) return acc;

    // Split list in head and tail
    T head = xs.get(0);
    List<T> tail = xs.subList(1, xs.size());

    // Recurse
    return f.apply(head, foldBack(f, tail, acc));
  }
  
  public static <T, U> U foldBack2(BiFunction<T, U, U> f, List<T> xs, U acc) {
    List<T> rev = new ArrayList<>(xs);
    Collections.reverse(rev);
    return rev.stream().reduce(acc,(u,x) -> f.apply(x,u),(a,b) -> b);
  }

  // Impleted as a recursive method
  public static <T, U> U fold(BiFunction<T, U, U> f, U acc, List<T> xs) {
    // Stop recursion
    if (xs.isEmpty()) return acc;

    // Split list in head and tail
    T head = xs.get(0);
    List<T> tail = xs.subList(1, xs.size());

    // Recurse
    return fold(f, f.apply(head, acc), tail);
  }
  
  public static <T, U> U fold2(BiFunction<T, U, U> f, U acc, List<T> xs) {
    return xs.stream().reduce(acc,(u,x) -> f.apply(x,u),(a,b) -> b);
  }
  
  public static void main(String[] args) {
    List<Integer> xs = List.of(4, -5, 6);
    printList(posList(xs));

    printList(squareList(xs));
    printList(add5List(xs));
    
    List<Pair<Integer,Integer>> xs2 = List.of(new Pair<Integer,Integer>(2,2),
                                              new Pair<Integer,Integer>(5,5));
    printList(addElems(xs2));

    printList(map(x -> x < 0 ? false : true, xs));
    printList(map(x -> x.fst + x.snd, xs2));

    printList(xs.stream().map(x -> x < 0 ? false : true).toList());
    printList(xs2.stream().map(x -> x.fst + x.snd).toList());

    List<Integer> xs3 = List.of(1, 3, 1, 4);
    printBool(xs3.stream().anyMatch(x -> x >= 2));
    printBool(isMember(3,xs3));

    printBool(forAll(x -> x < 10,xs3));

    printList(filter(x -> x < 3,xs3));

    // sum
    printInt(foldBack((x,a)->a+x, xs, 0));
    // length
    printInt(foldBack((x,a)->a+1, xs, 0));

    // sum
    printInt(foldBack2((x,a)->a+x, xs, 0));
    // length
    printInt(foldBack2((x,a)->a+1, xs, 0));

    // sum
    printInt(fold((x,a)->a+x, 0, xs));
    // length
    printInt(fold((x,a)->a+1, 0, xs));

    // sum
    printInt(fold2((x,a)->a+x, 0, xs));
    // length
    printInt(fold2((x,a)->a+1, 0, xs));

  }  
}
