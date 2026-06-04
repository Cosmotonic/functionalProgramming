class Pair<A, B> {
  public final A fst;
  public final B snd;

  public Pair(A fst, B snd) {
    this.fst = fst;
    this.snd = snd;
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
  
  // Exercise 2.1
  public static String dup(String s) {
    return s+s;
  }

  // Exercise 2.2
  public static String dupn(String s, int n) {
    String res = "";
    for (int i=0; i<n; i++)
      res = res + s;
    return res;
  }

  // Exercise 2.3
  public static int timediff(Pair<Integer,Integer> t1, Pair<Integer,Integer> t2) {
    return t2.fst*60+t2.snd - (t1.fst*60+t1.snd);
  }

  // Exercise 2.4
  public static int minutes(Pair<Integer,Integer> t) {
    Pair<Integer,Integer> t0 = new Pair<>(0,0);
    return timediff(t0,t);
  }

  // Exercise 2.4, HR 2.2
  public static String pow(String s, int n) {
    if (n==0)
      return "";
    else return s + pow(s,n-1);
  }
  
  // Exercise 2.5, HR 2.2
  public static String pow2(String s, int n) {
    String res = "";
    for (int i=n; i>0; i--)
      res += s;
    return res;
  }

  // Exercise 2.6, HR 2.8
  public static int bin(int n, int k) {
    if (k==0 || n==k)
      return 1;
    else
      return bin(n-1,k-1) + bin(n-1,k);
  }

  public static void main(String[] args) {
    printString(dup("Hi"));
    printString(dupn("Hi", 3));

    Pair<Integer,Integer> t1 = new Pair<>(12,34);
    Pair<Integer,Integer> t2 = new Pair<>(11,35);
    printInt(timediff(t1,t2));
    Pair<Integer,Integer> t3 = new Pair<>(13,35);
    printInt(timediff(t1,t3));

    Pair<Integer,Integer> t4 = new Pair<>(14,24);
    printInt(minutes(t4));
    Pair<Integer,Integer> t5 = new Pair<>(23,1);
    printInt(minutes(t5));

    printString(pow("Hi", 4));
    printString(pow2("Hi", 4));

    printInt(bin(0,0));
    printInt(bin(1,1));    
    printInt(bin(2,1));
    printInt(bin(4,2));
    
  }
}
