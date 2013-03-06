#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <dirent.h>
#include <linux/input.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <sys/time.h>
#include <termios.h>
#include <signal.h>
 
 
/*
http://www.kernel.org/doc/Documentation/input/event-codes.txt
http://stackoverflow.com/questions/2547616/how-can-i-translate-linux-keycodes-from-dev-input-event-to-ascii-in-perl
http://www.linuxquestions.org/questions/programming-9/read-from-a-usb-barcode-scanner-that-simulates-a-keyboard-495358/
http://www.thelinuxdaily.com/2010/05/grab-raw-keyboard-input-from-event-device-node-devinputevent/
http://code.metager.de/source/xref/linux/udev/src/keymap/keymap.c#print_key
http://lxr.free-electrons.com/source/drivers/hid/hid-input.c
*/
 
int
main(int argc, char *argv[]) {
	struct input_event ev[64];
	int fd;
	int rd;
	size_t size = sizeof(struct input_event);
	char name[256] = "Unknown";
	char *device = NULL;
 
	//Setup check
	if(argv[1] == NULL) {
		printf("Please specify (on the command line) the path to the dev event interface devicen\n");
		exit (0);
	}
 
	if((getuid ()) != 0) {
		printf("You have to be root\n");
		exit (0);
	}
 
	if(argc > 1) {
		device = argv[1];
	}
 
	//Open Device
	if((fd = open (device, O_RDONLY)) == -1) {
		printf("%s is not a vaild device.n", device);
		exit(0);
	}
 
	//Print Device Name
	ioctl(fd, EVIOCGRAB, name);
	ioctl(fd, EVIOCGNAME (sizeof (name)), name);
	printf("Reading From : %s (%s)\n", device, name);
 
	while(1) {
		if((rd = read(fd, ev, size * 64)) < size) {
			printf("read()");
			exit (0);
		}
 
		for(int i = 0; i < rd / size; i++) {
			printf("-- %i: %u\t%u\t%i\n", i, ev[i].type, ev[i].code, ev[i].value);
		}
	}
 
	return 0;
} 