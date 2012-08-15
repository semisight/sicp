#!/usr/bin/env python
import sys, os, re, json
from markdown2 import markdown as md

#TODO:
#1. Translate to javascript.
#2. Fetch input from github.com/semisight/sicp.
#3. Put on bitlimn under examples.

eval_md = True

if len(sys.argv) > 1 and sys.argv[1] == '-m':
	eval_md = False

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
	result = '\n'.join([handle_line(x) for x in (';##' + ex).splitlines()])

	if eval_md:
		return md(result)
	else:
		return result

def handle_file(val):
	result = re.split(';ex ', val)
	result = [handle_ex(x) for x in result]

	return '\n'.join(result)

source = {}

for (path, _, files) in os.walk('.'):
	for name in files:
		if os.path.splitext(name)[1] != '.rkt':
			continue

		with open(name) as f:
			source[name] = f.read()

for name, src in source.iteritems():
	if eval_md:
		new_ext = '.html'
	else:
		new_ext = '.md'

	with open(name + new_ext, 'w') as f:
		f.write(handle_file(src))
		print handle_file(src)

