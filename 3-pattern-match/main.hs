{-
-- Java code
 import java.io.*;
 import java.util.*;

 class Main {
   static int factorial (int n) {
     if (n == 0) {
       return 1;
     }
     return n * factorial (n - 1);
   }

   public static void main(String[] args) {
     System.out.println(factorial(10));
   }
 }
-}

factorial 0 = 1
factorial n = n * factorial (n - 1);

main = print (factorial 10)
