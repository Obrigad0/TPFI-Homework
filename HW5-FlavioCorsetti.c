#include <stdio.h>


struct Nodo{
	int valore;
	struct Nodo* left;
	struct Nodo* right;
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

// Il programma su un processore Apple M2 da risultato: Little Endian
// Il programma su un processore AMD da risultato: Little Endian

//

//Array Mutabili

void removeDups(int array[], int dim){
	int i = 0;
	struct Nodo* testa = creaNodo(array[i++]);
	for (i; i < dim; i++){
		
	}	
	
}

struct Nodo* creaNodo(int val){
	struct Nodo* nodo = (struct Nodo*)malloc(sizeof(struct Nodo));
	nodo -> valore = valore;
	nodo -> left = NULL;
	nodo -> right = NULL;
}

struct Nodo* inserisci(struct Nodo* nodo, int val){
	if (nodo == NULL){ //caso base
		return creaNodo(val);
	}
	if( val == nodo ->val){
		
	}
	if (val < nodo -> val){ // valore in input minore del valore nel nodo, vado a SX
		nodo -> left = inserisci(left, val);
	}else{
		
	}
}
int main() {
	endiannessValidation();
}



