#!/usr/bin/env python
from __future__ import print_function
import sys, json

#TODO:
#1. Translate to javascript.
#2. Fetch input from github.com/semisight/sicp.
#3. Put on bitlimn under examples.

def parse_ex(acc, line):
	if len(acc) == 0 and line[0:4] != ';ex ':
		return []

	if line[0:4] == ';ex ':
		acc.append([line])
	else:
		acc[-1].append(line)

	return acc

with open(sys.argv[1]) as f:
	inp = f.readlines();

print(json.dumps(reduce(parse_ex, inp, []), indent=2))