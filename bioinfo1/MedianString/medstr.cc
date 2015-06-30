#include <algorithm>
#include <iostream>
#include <string>
#include <list>

using namespace std;	

char nuc[] = {'A','C','G','T'};

int idx(char n) {
	switch(n) {
		case 'A' : return 0;
		case 'C' : return 1;
		case 'G' : return 2;
		case 'T' : return 3;
	}
	return -1;
}

string * inc(string * s) {
	string * result = new string(*s);
	int i;
	for ( i = s->length() - 1 ; i >= 0 ; i--) {
		int k = idx((*s)[i])+1;
		if ( k < 4 ) {
			(*result)[i] = nuc[k];
			break;
		} else {
			(*result)[i] = nuc[0];
		}
	}
	if (i<0)return NULL; else return result;
}

int hamming(string& s1, int i1, string& s2, int length) {
	int count = 0 ;
	for ( auto i = 0 ; i < length ; i ++ )
		if (s1[i1+i] != s2[i]) count ++ ;
	return count ;
}

int hammingIter(string& s1, string& s2, int length) {
	int min = INT_MAX;
	for ( int i = 0 ; i < s1.length() + length - 1 ; i ++ ) {
		int h = hamming(s1,i,s2,length);
		if (h < min) min = h;
	}
	return min;
}

string med(int k, string lines[], int t) {
	string * pat = new string(k,'A');
	int min = INT_MAX;
	string minPat;
	while (pat != NULL) {
		int hammingSum = 0 ;
		for (int i = 0 ; i < t ; i++) {
			hammingSum += hammingIter(lines[i],*pat,k);
		}
		if ( hammingSum < min ) {
			min = hammingSum;
			minPat = *pat;
		}
		pat = inc(pat);
	}
	return minPat ;
}

int main() {
	string line, lines[1000], dummy;
	int k;
	cin >> k ;
	getline(cin,dummy);
	getline(cin,line);
	lines[0] = line;
	int t = 1;
	while ( line != "") {
		getline(cin,line);
		lines[t++] = line;
	}
	t-- ;
	cout << med(k,lines,t) << endl;
}
