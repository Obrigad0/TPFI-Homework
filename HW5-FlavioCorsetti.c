#include <stdio.h>

int main() {
	endiannessValidation();
}


// 1. Type Safety

void endiannessValidation(){
	unsigned int intero = 1;
	char *c = (char*)&intero;
	if(*c == 1){
		printf("Little Endian system");
	}else{
		printf("Big Endian system");
	}
}

// Il programma su un processore Apple M2 da risultato: 
// Il programma su un processore AMD da risultato: Little Endian
