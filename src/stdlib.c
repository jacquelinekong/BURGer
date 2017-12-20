/* Standard Library for BURGer Programming Language
PLT Fall 2017
Includes 2 print functions (print and println)
Author: Adrian Traviezo */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>


// Prints given string and accepts escape characters
void print(char *s){
  char c;
  int i = 0;
  while (i < strlen(s)){
    c = s[i];
    if (c == '\\'){
      if (s[i+1] == 'n')
        printf("\n");
      if (s[i+1] == 't')
        printf("\t");
      i++;
    }
    else if (c == '\\' && s[i+1] == 't'){
      printf("\t");
      i++;
    }
    else
      printf("%c", c);
    i++;
  }
}

// as above, but with a newline at end
void println(char *s){
  char c;
  int i = 0;
  while (i < strlen(s)){
    c = s[i];
    if (c == '\\'){
      if (s[i+1] == 'n')
        printf("\n");
      if (s[i+1] == 't')
        printf("\t");
      i++;
    }
    else
      printf("%c", c);
    i++;
  }
  printf("\n");
}
