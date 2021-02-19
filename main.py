# SERBOI FLOREA-DAN 335CB

# importam acest modul pentru acces la parametri dati din linia de comanda
import sys

EPS = 'eps'

LOWEST_PRECEDENCE = 0
MEDIUM_PRECEDENCE = 5
HIGHEST_PRECEDENCE = 10

# pentru fiecare operator returnam o valoare caracteristica precedentei lor
def op_prec(operator):
	# operatorul de reuniune leaga cel mai slab
	if operator == '|':
		return LOWEST_PRECEDENCE
	# operatorul de concatenare leaga mai strans decat operatorul de reuniune
	# dar mai slab ca operatorul Kleene star
	elif operator == '.':
		return MEDIUM_PRECEDENCE
	# Kleene star leaga cel mai strans
	elif operator == '*':
		return HIGHEST_PRECEDENCE

# functia calculeaza forma postfixata a regex-ului
# elimina operatorii Kleene star consecutivi si parantezele
# si adauga explicit operatorul de concatenare
def parse_regex(regex):
	# construimm alfabetul si un nou regex in care:
	# - eliminam operatorii Kleene star consecutivi
	# - adaugam explicit operatorul de concatenare
	alphabet = set()
	i = 0
	regex_best_form = []
	while i < len(regex) :
		# adaugam simbolul curent,
		# exceptand cazul cand avem un Kleene star si urmeaza altul
		if not(i < len(regex) - 1 and regex[i] == '*' and regex[i + 1] == '*'):
			regex_best_form.append(regex[i])
			# daca nu avem paranteza deschisa sau reuniune,
			# si urmatorul simbol este o litera sau paranteza deschisa,
			# trebuie sa adaugam operatorul de concatenare
			if regex[i] not in {'(', '|'} and i < len(regex) - 1 \
			and regex[i+1] not in {')', '|', '*'}:
					regex_best_form.append('.')
			# daca avem o litera, o adaugam in alfabet
			if regex[i] not in {'(', ')', '|', '*'}:
				alphabet.add(regex[i])
		i += 1
	regex = regex_best_form

	# vom parsa expresia regulata obtinuta, calculand pentru ea forma postfixata
	postfix_regex = []
	# ne vom folosi de o stiva pentru a parsa regex-ul
	stack = []

	for symbol in regex :
		# daca avem o litera din alfabet, o adaugam la rezultat
		if symbol in alphabet :
			postfix_regex.append(symbol)
		# daca avem un operator
		elif symbol in { '|', '.', '*'} :
			# cat timp stiva nu este goala,
			# simbolul din varful stivei nu este o paranteza deschisa
			# si operatorul din varful stivei are precedenta mai mare sau
			# egala decat cea pentru operatorul curent
			while len(stack) > 0 and stack[-1] != '(' \
			and op_prec(stack[-1]) >= op_prec(symbol):
				# scoatem varful stivei si il adaugam la rezultat
				top_symbol = stack.pop()
				postfix_regex.append(top_symbol)
			# adaugam simbolul curent pe stiva
			stack.append(symbol)
		# daca avem o paranteza deschisa, o adaugam doar pe stiva,
		# nu si la rezultat(vrem sa eliminam parantezele)
		elif symbol == '(' :
			stack.append(symbol)
		# daca avem o paranteza inchisa
		elif symbol == ')' :
			# scoatem simbolurile de pe stiva si le adaugam la rezultat
			# pana cand dam de o paranteza deschisa, pe care nu o mai adaugam,
			# scapand de paranteze, care au doar rolul de a preciza precedenta
			while stack[-1] != '(' :
				top_symbol = stack.pop()
				postfix_regex.append(top_symbol)
			stack.pop()

	# simbolurile ramase pe stiva le adaugam la rezultat
	while len(stack) > 0 :
		top_symbol = stack.pop()
		postfix_regex.append(top_symbol)

	return postfix_regex, alphabet

# clasa de baza pentru un NFA
class NFA:
	def __init__(self, initial_state, final_state) :
		self.initial_state = initial_state
		self.final_state = final_state
		self.transitions = {}

# acest NFA accepta limbajul generat de expresia formata dintr-un simbol
# el are o tranzitie din starea initiala in starea finala pe simbol
class SymbolNFA(NFA):
	def __init__(self, initial_state, final_state, symbol) :
		self.initial_state = initial_state
		self.final_state = final_state

		self.transitions = {}
		self.transitions[initial_state, symbol] = [final_state]

# acest NFA accepta limbajul generat de expresia formata prin concatenarea
# a 2 subexpresii (pentru care am construit 2 NFA-uri
# care accepta limbajul generat de ele)
# noua stare initiala va fi starea initiala a primului NFA,
# iar noua stare finala va fi starea finala a celui de-al doilea NFA
# se introduce o tranzitie noua pe epsilon din starea finala a primului NFA
# catre starea initiala a celui de-al doilea NFA
class ConcatenationNFA(NFA):
	def __init__(self, nfa1, nfa2) :
		self.initial_state = nfa1.initial_state
		self.final_state = nfa2.final_state

		self.transitions = {**nfa1.transitions, **nfa2.transitions}

		self.transitions[nfa1.final_state, EPS] = [nfa2.initial_state]

# acest NFA accepta limbajul generat de expresia formata prin reuniunea
# a 2 subexpresii (pentru care am construit 2 NFA-uri
# care accepta limbajul generat de ele)
# sunt introduse 2 noi stari, una initiala si una finala
# noua stare initiala are tranzitii pe epsilon catre cele 2 stari initiale
# din cele 2 stari finale exista tranzitii pe epsilon catre noua stare finala
class ReunionNFA(NFA):
	def __init__(self, initial_state, final_state, nfa1, nfa2) :
		self.initial_state = initial_state
		self.final_state = final_state

		self.transitions = {**nfa1.transitions, **nfa2.transitions}

		self.transitions[initial_state, EPS] = [nfa1.initial_state,
												nfa2.initial_state]
		self.transitions[nfa1.final_state, EPS] = [final_state]
		self.transitions[nfa2.final_state, EPS] = [final_state]

# acest NFA accepta limbajul generat de aplicarea operatorului Kleene star pe o
# expresie pentru care am construit un NFA care accepta limbajul generat de ea
# sunt introduse 2 noi stari, una initiala si una finala
# atat noua stare initiala cat si starea finala a automatului au tranzitii
# pe epsilon catre starea initiala a automatului si catre noua stare finala
class KleeneStarNFA(NFA):
	def __init__(self, initial_state, final_state, nfa):
		self.initial_state = initial_state
		self.final_state = final_state

		self.transitions = nfa.transitions

		self.transitions[initial_state, EPS] = [nfa.initial_state, final_state]
		self.transitions[nfa.final_state, EPS] = [nfa.initial_state,
													final_state]

# algoritmul de transformare RE -> NFA
def postfix_regex_to_NFA(postfix_regex, alphabet):
	# stare disponibila pentru a fi adaugata in noul NFA
	avail_state = 0
	# vom folosi o stiva in care punem NFA-uri
	NFAs_stack = []
	for symbol in postfix_regex:
		# construim NFA-ul caracteristic fiecarui simbol,
		# dupa care il adaugam pe stiva
		if symbol in alphabet:
			# vom crea 2 noi stari in noul automat,
			# una find stare initiala, cealalta - finala
			initial_state = avail_state; avail_state += 1
			final_state = avail_state; avail_state += 1
			NFAs_stack.append(SymbolNFA(initial_state, final_state, symbol))
		elif symbol == '|':
			# vom crea 2 noi stari in noul automat,
			# una find stare initiala, cealalta - finala
			initial_state = avail_state; avail_state += 1
			final_state = avail_state; avail_state += 1
			# scoatem de pe stiva NFA-urile corespunzatoare subexpresiilor
			# pe care se aplica operatorul de reuniune
			nfa2 = NFAs_stack.pop()
			nfa1 = NFAs_stack.pop()
			NFAs_stack.append(ReunionNFA(initial_state, final_state,
											nfa1, nfa2))
		elif symbol == '.':
			# scoatem de pe stiva NFA-urile corespunzatoare subexpresiilor
			# pe care se aplica operatorul de concatenare
			nfa2 = NFAs_stack.pop()
			nfa1 = NFAs_stack.pop()
			NFAs_stack.append(ConcatenationNFA(nfa1, nfa2))
		elif symbol == '*':
			# vom crea 2 noi stari in noul automat,
			# una find stare initiala, cealalta - finala
			initial_state = avail_state; avail_state += 1
			final_state = avail_state; avail_state += 1
			# scoatem de pe stiva NFA-ul corespunzator expresiei
			# pe care se aplica Kleene star
			nfa = NFAs_stack.pop()
			NFAs_stack.append(KleeneStarNFA(initial_state, final_state, nfa))

	# la final obtinem NFA-ul care accepta limbajul generat de regex-ul initial
	return NFAs_stack.pop(), avail_state

# functia face ca starea 0 sa devina stare initiala in NFA
# astfel, interschimbam starea initiala cu starea 0 in tranzitii
def make_initial_state_0():
	# calculam tranzitiile noi pentru fiecare stare
	transitions_from_0 = {}
	transitions_from_initial_state = {}

	for state, symbol in list(nfa.transitions.keys()):
		# facem interschimbarea in multimea starilor urmatoare
		# daca putem ajunge in ambele stari, nu e nevoie sa schimbam
		if not(0 in nfa.transitions[state, symbol]
		and nfa.initial_state in nfa.transitions[state, symbol]):
			# daca ajungem doar in 0, schimbam
			if 0 in nfa.transitions[state, symbol]:
				nfa.transitions[state, symbol].remove(0)
				nfa.transitions[state, symbol].append(nfa.initial_state)
			# daca ajungem doar in starea initiala, schimbam
			elif nfa.initial_state in nfa.transitions[state, symbol]:
				nfa.transitions[state, symbol].remove(nfa.initial_state)
				nfa.transitions[state, symbol].append(0)
		# facem interschimbarea pentru starea din care se pleaca
		if state == 0:
			transitions_from_initial_state[nfa.initial_state, symbol] = \
												nfa.transitions[state, symbol]
			del nfa.transitions[state, symbol]
		elif state == nfa.initial_state:
			transitions_from_0[0, symbol] = nfa.transitions[state, symbol]
			del nfa.transitions[state, symbol]

	# noul NFA va fi format din tranzitiile existente si cele nou formate
	nfa.transitions = {**nfa.transitions, **transitions_from_0,
						**transitions_from_initial_state}

# functia scrie NFA-ul in fisierul dat ca parametru in linia de comanda
def write_NFA():
	# deschid fisierul in care voi scrie NFA-ul
	in_file = open(in_filename, "w")
	# scriu numarul de stari
	in_file.write(str(no_states) + '\n')
	# scriu starea finala
	in_file.write(str(nfa.final_state) + '\n')

	# scriu tranzitiile
	for state, symbol in nfa.transitions.keys():
		first = 1
		in_file.write(str(state) + " " + str(symbol) + " ")
		for next_state in nfa.transitions[state, symbol]:
			if first == 1:
				in_file.write(str(next_state))
				first = 0
			else:
				in_file.write(" " + str(next_state))
		in_file.write('\n')

	in_file.close()

#====================================TEMA 2====================================

# extrag informatiile legate de NFA din fisierul dat ca parametru
def read_NFA():
	# deschid fisierul de input
	in_file = open(in_filename, "r")
	# citesc numarul de stari al NFA-ului
	no_states = int(in_file.readline().split()[0])
	# citesc starile finale ale NFA-ului
	final_states = in_file.readline().split()
	# memorez alfabetul NFA-ului
	symbols = set()
	# calculez multimea starilor
	states_set = set()
	# citesc tranzitiile NFA-ului
	NFA_trans = {}
	for line in in_file:
		# citesc tranzitia din fisier
		line_transition = line.split()
		# adaug simbolul in set
		symbols.add(line_transition[1])
		# adaug tranzitia in dictionarul cu tranzitii,
		# cheia fiind starea de unde se face tranzitia
		# iar valoarea fiind o lista de perechi (simbol, lista stari urmatoare)
		curr_state_trans = NFA_trans.get(line_transition[0], [])
		curr_state_trans.append((line_transition[1], line_transition[2:]))
		NFA_trans[line_transition[0]] = curr_state_trans
		# actualizez multimea starilor
		states_set.add(line_transition[0])
		for s in line_transition[2:]:
			states_set.add(s)
	# inchid fisierul de input
	in_file.close()
	return no_states, final_states, symbols, states_set, NFA_trans

# functia calculeaza inchiderea epsilon pentru o stare
def eps_closure(state, NFA_trans):
	handled_states = set()
	unhandled_states = set()
	unhandled_states.add(state)
	while len(unhandled_states) > 0:
		curr_state = unhandled_states.pop()
		handled_states.add(curr_state)
		curr_state_trans = NFA_trans.get(curr_state, [])
		for symbol, next_states in curr_state_trans:
			if symbol == EPS:
				for next_state in next_states:
					if next_state not in handled_states:
						unhandled_states.add(next_state)
	return handled_states

# functia concateaza o multime de stari sub forma unui string
# starile fiind delimitate prin '/'
def compute_composed_state(states):
	composed_state = ""
	sorted_states = sorted(states)
	for i in range(len(sorted_states)):
		if i != 0:
			composed_state += '/'
		composed_state += sorted_states[i]
	return composed_state

# functia construieste DFA-ul
def compute_DFA(final_states, NFA_trans):
	# construim starea initiala a DFA-ului
	# care reprezinta inchiderea epsilon a starii initiale din NFA
	# o memoram ca string, concatenand starile, delimitate prin '/'
	init_state_eps_closure = eps_closure('0', NFA_trans)
	DFA_initial_state = compute_composed_state(init_state_eps_closure)
	DFA_trans = {}
	DFA_final_states = set()
	# vom tine in memorie multimea starilor neprelucrate si a celor prelucrate
	unhandled_states = set()
	handled_states = set()
	# vom porni de la starea initiala
	unhandled_states.add(DFA_initial_state)
	# cat timp avem stari neprelucrate
	while(len(unhandled_states) > 0):
		# extragem o stare neprelucrata, construim tranzitiile in DFA
		composed_state = unhandled_states.pop()
		handled_states.add(composed_state)
		composed_state_list = str(composed_state).split('/')
		# starea compusa este stare finala in DFA
		# daca cel putin o stare continuta este stare finala in NFA
		for s in composed_state_list:
			if s in final_states:
				DFA_final_states.add(composed_state)
		# calculam tranzitiile pentru aceasta stare compusa
		trans = {}
		for curr_state in composed_state_list:
			curr_state_trans = NFA_trans.get(curr_state, [])
			# parcurgem tranzitiile starii curente din multimea starii compuse
			for symbol, next_states in curr_state_trans:
				if symbol != EPS:
					t = trans.get(symbol, [])
					# adaugam starile urmatoare in multimea starilor urmatoare
					# pentru starea compusa curenta
					for n_st in next_states:
						t.append(n_st)
						n_st_closure = eps_closure(n_st, NFA_trans)
						t[0:0] = n_st_closure
					trans[symbol] = list(set(t))
		# inlocuim multimile starilor urmatoare
		# cu reprezentarea lor concatenata ca string
		for symbol in trans.keys():
			value = trans.get(symbol, [])
			new_composed_state = compute_composed_state(value)
			trans[symbol] = new_composed_state
			# daca starea noua compusa nu se afla printre cele tratate
			if new_composed_state not in handled_states:
				# adaugam starea in multimea celor netratate
				unhandled_states.add(new_composed_state)
		# completam tabelul de tranzitii pentru starea compusa curenta
		DFA_trans[composed_state] = trans
	return DFA_final_states, DFA_trans

# functia scrie in fisier starile finale ale DFA-ului
def write_final_states(out_file, DFA_final_states, states_map):
	first = 1
	for final_state in DFA_final_states:
		if first == 1:
			first = 0
			out_file.write(states_map[final_state])
		else:
			out_file.write(' ' + states_map[final_state])
	out_file.write('\n')

# functia scrie in fisier tranzitiile DFA-ului
def write_transitions(out_file, DFA_trans, states_map, symbols, curr_no):
	for state in DFA_trans.keys():
		transition = DFA_trans[state]
		for symbol in symbols:
			out_file.write(states_map[state] + ' ')
			n_st = transition.get(symbol, '-1')
			# verificam daca exista tranzitie pe simbolul curent
			if n_st != '-1':
				out_file.write(symbol + ' ' + states_map[n_st])
			# altfel, ajungem in sink state
			else:
				out_file.write(symbol + ' ' + str(curr_no))
			out_file.write('\n')
	# tranzitiile pentru sink state
	for symbol in symbols:
		out_file.write(str(curr_no) + ' ' + symbol + ' ' + str(curr_no) + '\n')

# scriu informatiile legate de DFA in fisierul de output
def write_DFA():
	# deschid fisierul de output
	out_file = open(out_filename, "w")
	# scriu in fisier numarul de stari pentru DFA
	# la care se mai adauga starea sink care va fi construita ulterior
	out_file.write(str(len(DFA_trans.keys()) + 1) + '\n')
	# mapez starile obtinute la intregi
	curr_no = 0
	states_map = {}
	for state in DFA_trans.keys():
		states_map[state] = str(curr_no)
		curr_no = curr_no + 1
	# scriu starile finale in fisier
	write_final_states(out_file, DFA_final_states, states_map)
	# nu avem tranzitii pe simbolul epsilon intr-un DFA
	if EPS in symbols:
		symbols.remove(EPS)
	# scriu tranzitiile DFA-ului
	write_transitions(out_file, DFA_trans, states_map, symbols, curr_no)
	# inchid fisierul de output
	out_file.close()

#====================================TEMA 2====================================

if __name__ == "__main__":
	# extrag denumirile fisierelor date din linia de comanda
	regex_filename = sys.argv[1]
	in_filename = sys.argv[2]
	out_filename = sys.argv[3]

	# deschid fisierul cu expresia regulata
	regex_file = open(regex_filename, "r")

	# citesc regex-ul din fisier
	regex = list(regex_file.readline())

	# parsez expresia regulata
	postfix_regex, alphabet = parse_regex(regex)

	# calculez NFA-ul
	nfa, no_states = postfix_regex_to_NFA(postfix_regex, alphabet)

	# daca starea initiala nu e 0, inlocuim starea initiala cu 0 si invers
	# in tranzitii atat pentru starea initiala cat si pentru starilor urmatoare
	if nfa.initial_state != 0:
		make_initial_state_0()

	# scriem NFA-ul in fisierul dat ca parametru in linia de comanda
	write_NFA()

	# extrag numarul de stari, starile finale, simbolurile, multimea starilor
	# si tranzitiile NFA-ului din fisier
	no_states, final_states, symbols, states_set, NFA_trans = read_NFA()

	# calculez inchiderea epsilon pentru fiecare stare a NFA-ului
	eps_closure_all_states = {}
	for st in NFA_trans.keys():
		eps_closure_all_states[st] = eps_closure(st, NFA_trans)

	# calculez DFA-ul
	DFA_final_states, DFA_trans = compute_DFA(final_states, NFA_trans)

	# scriu numarul de stari, starile finale si tranzitiile DFA-ului
	# in fisierul de ouput
	write_DFA()
