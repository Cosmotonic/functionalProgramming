class Main {

  public static void printInt(int n) {
    System.out.printf("%d \n", n);
  }

  public static void printDouble(double n) {
    System.out.printf("%.2f \n", n);
  }

  // Exercise 1.1
  public static int sqr(int x) {
    return x*x;
  }

  // Exercise 1.2
  public static double pow(double x, double n) {
    return Math.pow(x,n);
  }

  // Exercise 1.3, HR 1.1
  public static int g(int n) {
    return n+4;
  }

  // Exercise 1.4, HR 1.2  
  public static double h(double x, double y) {
    return Math.sqrt(x*x+y*y);
  }

  // Exercise 1.5, HR 1.4
  public static int f(int n) {
    if (n==0)
      return 0;
    else
      return (n + f(n-1));
  }

  // Exercise 1.6, HR 1.5
  public static int fib(int n) {
    switch (n) {
      case 0: return 0;
      case 1: return 1;
      default: return fib(n-1) + fib(n-2);
    }
  }

  // Exercise 1.7, HR 1.6
  public static int sum(int m, int n) {
    if (n==0)
      return m;
    else
      return m+n+sum(m,n-1);
  }

  public static void main(String[] args) {
    printInt(sqr(4));
    printDouble(pow(4.0,2.0));
    printInt(g(4));
    printDouble(h(4.0,5.0));
    printInt(f(4));
    printInt(fib(10));
    printInt(sum(5,10));    
  }

}
