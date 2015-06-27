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

int count(string& line, string& pattern, int dist) {
	int c = 0;
	for ( int i = 0; i < line.length() - pattern.length() + 1; i ++ )
		if ( hamming(line,i,pattern,pattern.length()) <= dist) {
			c ++ ;
		}
	return c;
}

int main() {
	string line;
	int k, d;
	getline(cin,line);
	cin >> k >> d ;
	string * pat = new string(k,'A');
	list<string> sol;
	int maxfreq = 0;
	while (pat != NULL) {
		int c = count(line,*pat,d);
		if ( c > maxfreq ) {
			maxfreq = c;
			sol.clear();
			sol.push_back(*pat);
		} else if (c == maxfreq) {
			sol.push_back(*pat);
		}
		pat = inc(pat);
	}
	for(auto i = sol.begin() ; i != sol.end() ; i ++ ) cout << *i << " ";
	cout << endl;
}
