#include <algorithm>
#include <iostream>
#include <string>
#include <set>

using namespace std;	

char rev(char n) {
	switch (n) {
		case 'A' : return 'T';
		case 'C' : return 'G';
		case 'G' : return 'C';
		case 'T' : return 'A';
	}
}

string revcomp(const string&  s) {
	string result((int)s.length(),' ');
	for ( int i = 0 ; i < s.length() ; i++ )
		result[s.length()-1-i] = rev(s[i]);
	return result;
}

char nuc[] = {'A','C','G','T'};

int idx(char n) {
	switch(n) {
		case 'A' : return 0;
		case 'C' : return 1;
		case 'G' : return 2;
		case 'T' : return 3;
	}
}

string * inc(string const * const s) {
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

int hamming(const string& s1, int i1, const string& s2, int length) {
	int count = 0 ;
	for ( auto i = 0 ; i < length ; i ++ )
		if (s1[i1+i] != s2[i]) count ++ ;
	return count ;
}

int count(const string& line, const string& pattern, int dist) {
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
	const string * pat = new string(k,'A');
	set<string> sol;
	int maxfreq = 0;
	int z = 0;
	while (pat != NULL) {
		const string r = revcomp(*pat);
		if (sol.find(r) == sol.end()) {
			int c = count(line,*pat,d);
			c += count(line,r,d);
			if ( c > maxfreq ) {
				maxfreq = c;
				sol.clear();
				sol.insert(*pat);
			} else if (c == maxfreq) {
				sol.insert(*pat);
			}
		}
		pat = inc(pat);
	}
	for(auto i = sol.begin() ; i != sol.end() ; i ++ ) {
		cout << *i << " " << revcomp(*i) << " ";
	}
	cout << endl;
}
