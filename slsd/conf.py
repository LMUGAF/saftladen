from log import Log
from log import LogStdoutSink
from log import LogFilterRule
from log import LogFileSink
from log import LogSyslogSink
from xml.etree.ElementTree import ElementTree
from daemon import Daemon
import aux


## Element parsing
#### Boolean
def boolVal(text):
	text = text.lower()
	
	if text in ("t", "true", "y", "yes"):
		return True
	elif text in ("f", "false", "n", "no"):
		return False
	else:
		raise Exception("Not a boolean")


def boolAttrib(xmlEl, attrib, default = None):
	if xmlEl.get(attrib) is None:
		return default
	return boolVal(xmlEl.get(attrib))


def boolTag(xmlEl, default = None):
	if xmlEl is None:
		return default
	return boolVal(xmlEl.text)

#### Integer
def intVal(text, default = None):
	if text.isdigit():
		return int(text)
	else:
		raise Exception("Not a integer")


def intAttrib(xmlEl, attrib, default = None):
	if xmlEl.get(attrib) is None:
		return default
	return intVal(xmlEl.get(attrib), default)


def intTag(xmlEl, default = None):
	if xmlEl is None:
		return default
	return intVal(xmlEl.text, default)







def read(file):
	tree = ElementTree()
	tree.parse(file)
	return tree

## Loging
def logFileSinks(tree):
	for element in tree.findall("./log/file"):
		sink = LogFileSink(
			element.get("path"),
			element.get("mode"),
			element.get("user"),
			element.get("group")
		)
		logSink(element, sink)


def logSyslogSink(tree):
	element = tree.find("./log/syslog")
	if element is not None:
		logSink(element, LogSyslogSink())


def logStdoutSink(tree):
	element = tree.find("./log/stdout")
	if element is not None:
		logSink(element, LogStdoutSink())


def logSink(xmlEl, sink):
	## Time format
	timeFmltEl = xmlEl.find("./timeformat")
	if timeFmltEl is not None:
		sink.setTimeFormat(timeFmltEl.text)
	
	## Filter rules
	for filterEl in xmlEl.findall("./filter"):
		levels   = logGroup(filterEl, "level",   Log.LVL)
		services = logGroup(filterEl, "service", Log.SRV)
		
		log = boolAttrib(filterEl, "log")
		
		filterRule = LogFilterRule(levels, services, log)
		sink.addFilter(filterRule)
	
	## Finally add the sink to the logger
	Log.addSink(sink)


def logGroup(xmlEl, name, group):
	result = []
	fullText = xmlEl.get(name)
	
	## Wildcard (all group items)
	if fullText == "*":
		result = group.getAll()
	
	## List of group items
	else:
		for partText in ",".split(fullText):
			partText = partText.strip()
			part = group.getByName(serviceText)
			result.append(part)
	
	return result


## System
def daemon(tree):
	d = Daemon()
	
	el = tree.find("./user")
	if el is not None:
		d.user = el.text
	
	el = tree.find("./group")
	if el is not None:
		d.group = el.text
	
	el = tree.find("./pidfile")
	if el is not None:
		d.pidfile  = el.text
		d.pidmode  = el.get("mode")
		d.piduser  = el.get("user")
		d.pidgroup = el.get("group")
	
	el = tree.find("./errfile")
	if el is not None:
		## This file descriptor is istill not recreatet on SIGHUP
		errFile = open(el.text, "a", 1)
		aux.setFileAccess(el.text, el.get("mode"), el.get("user"), el.get("group"))
		
		d.stdout = errFile
		d.stderr = errFile
	
	return d
	

