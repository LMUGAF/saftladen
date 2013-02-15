import threading
import subprocess
from log import Log


def sh(cmd, timeout):
	proc = subprocess.Popen(
		cmd,
		stderr = subprocess.PIPE,
		stdout = subprocess.PIPE
	)
	
	def processThreadFunc():
		for line in proc.stdout.readlines():
			Log.log(Log.LVL_INFO, Log.SRV_PROC, (line[:-1]).decode('utf-8'))
	
	def stopThreadFunc():
		proc.terminate()
	
	processThread = threading.Thread(target = processThreadFunc)
	processThread.start()
	
	stopThread = threading.Timer(timeout, stopThreadFunc)
	stopThread.start()
