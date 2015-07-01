#include <algorithm>
#include <iostream>
#include <string>
#include <set>

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

int hamming(const string& s1, int i1, const string& s2, const int i2, const int length) {
	int count = 0 ;
	for ( auto i = 0 ; i < length ; i ++ )
		if (s1[i1+i] != s2[i2+i]) count ++ ;
	return count ;
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

set<int> countBits(unsigned long long z) {
	set<int> result;
	for ( int i = 0 ; i < 64 ; i ++, z >>= 1 ) {
		if ( z & 1 ) result.insert(i) ;
	}
	return result;
}

set<string> neighbors(string& s, int d) {
	set<string> result;
	for ( unsigned long long z = 1 ; z < (1 << s.length()) - 1 ; z ++) {
		set<int> positions = countBits(z);
		if ( positions.size() <= d ) {
			string *x = new string(d,'A');
			while ( x != NULL ) {
				string m(s);
				int j = 0;
				for ( auto i = positions.begin() ; i != positions.end() ; i++, j++ ) {
					m[*i] = (*x)[j];
				}
				result.insert(m);
				x = inc(x);
			}
		}
	}
	return result;
}

int count(
		const string& line, const string& pattern, 
		const int offset, const int length, const int dist) {
	int c = 0;
	for ( int i = 0; i < line.length() - length + 1; i ++ ) {
		if ( hamming(line,i,pattern,offset,length) <= dist ) {
			c ++ ;
		}
	}
	return c;
}

set<string> motifEnumeration(
		const int k, const int d, const string * const lines, const int nl) {
	set<string> result;
	for ( unsigned i = 0 ; i < lines[0].size() - k + 1 ; i ++ ) {
		string x(lines[0],i,k);
		set<string> n = neighbors(x,d);
		for ( auto l = n.begin() ; l != n.end() ; l++ ) {
			bool isMotif = true ;
			for ( unsigned j = 1 ; j < nl ; j ++ ) {
				int c = count(lines[j], *l, 0, k, d);
				if ( c == 0 ) {
					isMotif = false;
					break;
				}
			}
			if(isMotif) {
				result.insert(*l);
			}
		}
	}
	return result;
}

int main() {
	string s("ACGT");
	cout << neighbors(s,3).size() << endl ;
}