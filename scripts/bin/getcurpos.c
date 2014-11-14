#include<stdio.h>
#include<stdlib.h>
#include<X11/Xlib.h>

int main(int argc, char *argv[])
{
	XEvent e;
	Display *d = XOpenDisplay(NULL);

	if(!d)
		return EXIT_FAILURE;

	/* get info about current pointer position */
	XQueryPointer(d, RootWindow(d, DefaultScreen(d)),
			&e.xbutton.root, &e.xbutton.window,
			&e.xbutton.x_root, &e.xbutton.y_root,
			&e.xbutton.x, &e.xbutton.y,
			&e.xbutton.state);

	printf("%d %d\n", e.xbutton.x, e.xbutton.y);

	XCloseDisplay(d);

	return EXIT_SUCCESS;
}
