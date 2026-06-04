// Java example for lecture 02.

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


  //Slides on Statemnets and Expressions
  public static String Ex01(int x) {
    int n = 5;
    if (x > n)
      return "x>5";
    else
      return "x<5";
  }

    //Slides on Statemnets and Expressions
  // public static String Ex02(int x) {
  //   return
  //     "The result is " +
  //     n = 5;
  //     if (n > 5)
  //       return "x>5";
  //     else
  //       return "x<5";      
  // }

  //Slides on Statemnets and Expressions
  public static String Ex03() {
    int n = 5;
    return "The result is " + (n > 5 ? "x>5" : "x<5");
  }
  
  public static void main(String[] args) {
    printString(Ex01(8));

    printString(Ex03());
  }
}
