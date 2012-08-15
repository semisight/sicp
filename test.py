#!/usr/bin/env python
import os, re, json
from markdown2 import markdown as md

#TODO:
#1. Translate to javascript.
#2. Fetch input from github.com/semisight/sicp.
#3. Put on bitlimn under examples.

def handle_line(line):
	result = line

	if len(line) == 0:
		return result

	if line[0] == ';':
		result = line[1:]
	else:
		result = '    ' + line

	return result

def handle_ex(ex):
	result = [handle_line(x) for x in (';##' + ex).splitlines()]

	return '\n'.join(result)

def handle_file(val):
	result = re.split(';ex ', val)
	result = [md(handle_ex(x)) for x in result]

	return '\n'.join(result)

source = {}

for (path, _, files) in os.walk('.'):
	for name in files:
		if os.path.splitext(name)[1] != '.rkt':
			continue

		with open(name) as f:
			source[name] = f.read()

for name, src in source.iteritems():
	with open(name+'.html', 'w') as f:
		f.write(handle_file(src))
		print handle_file(src)

