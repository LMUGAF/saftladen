from log import Log
from log import LogStdoutSink
from log import LogFilterRule
from log import LogFileSink
from log import LogSyslogSink
from route import Route
from xml.etree.ElementTree import ElementTree
from daemon import Daemon
import aux

class XmlMetaPlugin(type):
	def __init__(self, class_name, bases, namespace):
		if not hasattr(self, '_plugins'):
			self._plugins = {}
		else:
			self._plugins[self.elName] = self
			
			if not hasattr(XmlMetaPlugin, '_elNames'):
				XmlMetaPlugin._elNames = ["log", "user", "group", "pidfile", "errfile"]
			else:
				if self.elName in XmlMetaPlugin._elNames:
					raise Exception("There is already a registered XML element \"%s\"" % self.elName)
			XmlMetaPlugin._elNames.append(self.elName)
			Log.log(
				Log.LVL_INFO,
				Log.SRV_MISC,
				"%s registerd a new %s for XML element \"%s\"" % (
					class_name,
					bases[0].__name__,
					self.elName
				)
			)
			
			
			Log.log(
				Log.LVL_DEBUG2,
				Log.SRV_MISC,
				"There are now the following %s: %s" % (
					bases[0].__name__,
					", ".join(self._plugins)
				)
			)
			
			Log.log(
				Log.LVL_DEBUG2,
				Log.SRV_MISC,
				"There are now the following XML elements: %s" % (
					", ".join(XmlMetaPlugin._elNames)
				)
			)
	
	def getPlugins(self):
		return self._plugins



class XmlPlugin():
	def foo(cls, tree, router, scanners = None):
		plugins = cls.getPlugins()
		res = {}
		
		for key in plugins:
			Log.log(Log.LVL_DEBUG2, Log.SRV_MISC, "Looking for \"%s\"" % key)
			
			for element in tree.findall("./%s" % key):
				name = element.get("name")
				if name is None:
					name = "default"
				
				if name in res:
					raise Exception("There is already a \"%s\" handler" % name)
				
				
				handler = plugins[key].create(element, name)
				
				if not scanners is None:
					for portEl in element.findall("./port"):
						port = handler.getPort(portEl.get("name"))
						
						for routeEl in portEl.findall("./route"):
							routeScanners = []
							scannersStr = routeEl.get("scanner")
							domain = routeEl.get("domain")
							
							if domain is None:
								domain = ""
							
							if scannersStr is None or scannersStr == "*":
								for scannerName in scanners:
									routeScanners.append(scanners[scannerName])
							else:
								for scannerName in scannersStr.split(","):
									routeScanners.append(scanners[scannerName])
							print("Add route for %s of %s for port %s and domain %s" % (name, key, portEl.get("name"), domain))
							route = Route(routeScanners, domain, port)
							router.addRoute(route)
				else:
					handler.addListener(router)
				
				res[name] = handler
				
				Log.log(Log.LVL_INFO, Log.SRV_MISC, "Added \"%s\" scanner named \"%s\"" % (key, name))
		
		return res

class ScannerPlugin(XmlPlugin, metaclass=XmlMetaPlugin):
	def init(tree, router):
		return XmlPlugin.foo(ScannerPlugin, tree, router)

class HandlerPlugin(XmlPlugin, metaclass=XmlMetaPlugin):
	def init(tree, router, scanners):
		return XmlPlugin.foo(HandlerPlugin, tree, router, scanners)




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
		for partText in fullText.split(","):
			partText = partText.strip()
			part = group.getByName(partText)
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
	

