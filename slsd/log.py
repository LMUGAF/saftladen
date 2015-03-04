from datetime import datetime
from threading import Lock
import syslog
import aux
import os
import stat



class LogGroupItem:
	def __init__(self, group = None, name = "", itemId = 0, color = 0):
		self.group = group
		self.name = name
		self.itemId = itemId
		self.color = color
	
	
	def __str__(self):
		return self.group.format(self)
		return self.name + " " * (self.group.maxName - len(self.name))



class LogGroup:
	(COLOR_NONE, COLOR_BEFORE, COLOR_AFTER, COLOR_TEXT) = range(4)
	
	
	def __init__(self, colorPos = COLOR_BEFORE):
		self.maxName = 0
		self.lastId = 0
		self.items = []
		self.colorPos = colorPos
	
	
	def add(self, name, color):
		item = LogGroupItem(self, name, self.lastId, color)
		self.lastId = self.lastId + 1
		self.items.append(item)
		
		if len(name) > self.maxName:
			self.maxName = len(name)
		
		return item
	
	
	def format(self, item):
		name = item.name + " " * (self.maxName - len(item.name))
		
		if self.colorPos == LogGroup.COLOR_NONE:
			return name
		elif self.colorPos == LogGroup.COLOR_BEFORE:
			return "\033[%im \033[0m %s" % (item.color + 40, name)
		elif self.colorPos == LogGroup.COLOR_AFTER:
			return "%s \033[%im \033[0m" % (name, item.color + 40)
		elif self.colorPos == LogGroup.COLOR_TEXT:
			return "\033[1;%im%s\033[0m" % (item.color + 30, name)
	
	
	def getByName(self, name):
		name = name.lower()
		
		for item in self.items:
			if item.name.lower() == name:
				return item
		
		return None
	
	
	def getAll(self):
		return self.items



class LogFilterRule:
	def __init__(self, levels = [] , services = [], log = None):
		self.levels = levels
		self.services = services
		self.log = log
	
	
	def filterResult(self, level, service):
		if level in self.levels and service in self.services:
			return self.log
		
		return None



class LogSink:
	def __init__(self):
		self.filterRules = []
	
	
	def addFilter(self, filterRule):
		self.filterRules.append(filterRule)
	
	
	def log(self, level, service, msg):
		for filterRule in self.filterRules:
			filterResult = filterRule.filterResult(level, service)
			
			if filterResult == True:
				self.execLog(level, service, msg)
				break
			elif filterResult == False:
				break


class LogFullColorSink(LogSink):
	def __init__(self, onlyMsg = False):
		LogSink.__init__(self)
		
		self.timeFmt = None
		self.onlyMsg = onlyMsg
	
	def setTimeFormat(self, fmt = '%Y-%m-%d %H:%M:%S'):
		self.timeFmt = fmt
	
	def setOnlyMsg(self, onlyMsg = True):
		self.onlyMsg = onlyMsg
	
	
	def execLog(self, level, service, msg):
		elements = []
		if self.timeFmt is not None:
			elements.append(datetime.now().strftime(self.timeFmt))
		
		if self.onlyMsg == False:
			elements.append(str(level))
			elements.append(str(service))
		
		elements.append(msg)
		
		self.write("  ".join(elements))



class LogFileSink(LogFullColorSink):
	def __init__(self, path, mode = None, user = None, group = None, onlyMsg = False):
		LogFullColorSink.__init__(self, onlyMsg)
		
		self.path = path
		self.mode = mode
		self.user = user
		self.group = group
		
		self.logfile = None
		self.fileLock = Lock()
		self.openFile()
	
	
	def openFile(self):
		with self.fileLock:
			if self.logfile is not None:
				self.logfile.close()
			
			## buffersize "1" means "line buffered"
			if os.path.exists(self.path) and stat.S_ISFIFO(os.stat(self.path).st_mode):
				self.logfile = open(self.path, 'w', 1)
			else:
				self.logfile = open(self.path, 'a', 1)
				aux.setFileAccess(self.path, self.mode, self.user, self.group)
	
	
	def write(self, line):
		with self.fileLock:
			self.logfile.write(line + "\n")



class LogStdoutSink(LogFullColorSink):
	def write(self, line):
		print(line)


## http://pubs.opengroup.org/onlinepubs/007908799/xsh/syslog.h.html
class LogSyslogSink(LogSink):
	instance = None
	
	
	def __init__(self, facility = syslog.LOG_DAEMON):
		LogSink.__init__(self)
		
		if LogSyslogSink.instance is None:
			LogSyslogSink.instance = self
			syslog.openlog("slsd", syslog.LOG_PID, facility)
		else:
			raise Exception("There can only be one syslog log sink")
	
	
	def execLog(self, level, service, msg):
		if level == Log.LVL_DEBUG:
			sysLevel = syslog.LOG_DEBUG
		
		elif level == Log.LVL_NOTICE:
			sysLevel = syslog.LOG_NOTICE
		
		elif level == Log.LVL_ERROR:
			sysLevel = syslog.LOG_ERR
		
		else:
			raise Exception("Can't translate log level to syslog level")
		
		syslog.syslog(sysLevel, "  ".join((str(level), str(service), msg)))



class Log:
	sinks = []
	
	LVL = LogGroup(LogGroup.COLOR_BEFORE)
	LVL_DEBUG1    = LVL.add("Debug 1", 4)
	LVL_DEBUG2    = LVL.add("Debug 2", 6)
	LVL_INFO      = LVL.add("Info",    2)
	LVL_NOTICE    = LVL.add("Notice",  3)
	LVL_WARN      = LVL.add("Warn",    5)
	LVL_DISPLAY   = LVL.add("Display", 3)
	LVL_ERROR     = LVL.add("Error",   1)
	
	SRV = LogGroup(LogGroup.COLOR_TEXT)
	SRV_MAIN      = SRV.add("Main",    1)
	SRV_SCAN      = SRV.add("Scan",    2)
	SRV_ROUTE     = SRV.add("Router",  3)
	SRV_HANDLER   = SRV.add("Handler", 4)
	SRV_DEB_H     = SRV.add("Deb. H.", 5)
	SRV_PROC      = SRV.add("Proc",    6)
	SRV_MISC      = SRV.add("Misc",    7)
	
	
	def addSink(sink):
		Log.sinks.append(sink)
	
	
	def log(level, service, msg):
		for sink in Log.sinks:
			sink.log(level, service, msg)

