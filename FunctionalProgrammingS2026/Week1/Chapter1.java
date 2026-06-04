// Java examples to HR, Chapter 1
import java.util.function.DoubleUnaryOperator;
import java.util.function.ToIntFunction;
import java.util.function.IntPredicate;

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

  // Slide: Imperative models
  public static void Ex01() {
    int i = 0;
    int s = 0;
    int[] a = {1, 2, 3, 4, 5};

    while (i < a.length) {
      s += a[i];
      i += 1;
    }
    System.out.printf("%d \n", s);
  }

  //Slide: Value Declarations
  public static boolean Ex02() {
    int price;
    price = 25*5;

    int newPrice;
    newPrice = 2*price;

    boolean b = newPrice > 500;
    return b;
  }

  //Slide: Anonymous functions
  public static DoubleUnaryOperator mkCircleArea() {
    return r -> Math.PI * r * r;
  }

  //Slide: Anonymous functions
  public static ToIntFunction<Boolean> mkBoolToInt() {
    return b -> b?1:0;
  }

  //Slide: Anonymous functions
  public static IntPredicate mkIntToBool() {
    return i -> (i==0)?false:true;
  }

  //Slide: Recursive declaration. Example n!
  public static int fact(int n) {
    if (n==0)
      return 1;
    else
      return n * fact(n-1);
  }

  public static int factImperative(int n) {
    int res = 1;
    for (int i=2; i<=n; i++)
      res *= i;
    return res;
  }

  //Slide: Recursive declaration. Example x^n
  public static double power(double x, int n) {
    if (n==0)
      return 1.0;
    else
      return x * power(x,n-1);
  }

  public static double powerImperative(double x, int n) {
    double res = 1.0;
    for (int i=1; i<=n; i++)
      res *= x;
    return res;
  }
  
  public static void main(String[] args) {
    Ex01();

    printBool(Ex02());

    printDouble(mkCircleArea().applyAsDouble(2.0));

    printInt(mkBoolToInt().applyAsInt(true));
    printInt(mkBoolToInt().applyAsInt(false));

    printBool(mkIntToBool().test(0));
    printBool(mkIntToBool().test(1));

    printInt(fact(3));
    printInt(factImperative(3));

    printDouble(power(4.0,2));
    printDouble(powerImperative(4.0,2));    

  }
}
