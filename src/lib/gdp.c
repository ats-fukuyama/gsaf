/* $Id$ */
#ifdef SONYCISC
#define BSD
#endif

#ifdef SUN
#define BSD
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#ifndef BSD
#include <termio.h>
#else
#include <sgtty.h>
#endif

#ifndef SONYCISC
#include <stdlib.h>
#else
extern char *getenv();
#define M_PI	3.14159265358979323846
#endif

#ifdef LONGINT
#define		int4	int
#else
#define		int4	long
#endif

#define DEG	(M_PI/180)
#define NBUFMAX	255
#define NPNTMAX	256

static Display	        *display;
static Window	        window,focus,rootw,parentw;
static GC		gc;
static int		screen;
static Cursor	        curs;
static Colormap		cm;
static int		mapped;
static int		gray,fullcolor,whitezero;
static int		xpos,ypos,ymax;
static float		delta;
static int		dxch,dych;
static int		ilns,ibls,icls;
static int		rmode,tmode;
static int		chkeymode;
static int		pageno;
static int		igs,ihr,ix1p,ix2p,iy1p,iy2p,ixyp;
static double		cgamma;
static int		nbuf;
static char		cbuf[NBUFMAX+1];

static XPoint		points[NPNTMAX];

#define CCOLSIZE 8
static XColor ccol[CCOLSIZE];
#define DCOLSIZE 256
static XColor dcol[DCOLSIZE];
static unsigned long dpixel[DCOLSIZE];
static int	nccol,ndcol;


static void tinit()
{
	nbuf = 0;
}

static void tbuff()
{
	cbuf[nbuf] = 0;
	printf("%s",cbuf);
	fflush(stdout);
	nbuf = 0;
}

static void toutpt(i)
int	i;
{
	if(nbuf+1 >= NBUFMAX)
		tbuff();
	cbuf[nbuf] = (char)i;
	nbuf++;;
}

static void toutst(n,ich)
int	n;
char	ich[];
{
	int	i;
	
	if(nbuf + n  >= NBUFMAX)
		tbuff();
	for(i = 0; i < n; i++)
		cbuf[nbuf+i] = ich[i];
	nbuf += n;
}

static void dvsetwin(win,fwin)
Window win,fwin;
{
        XWindowAttributes attribs;

	/*
	printf(" !! dvsetwin:win,focus= %d,%d\n",win,fwin);
	*/
	fflush(stdout);
	if(win != rootw) {
		XMapRaised(display,win);
		do {
			XGetWindowAttributes(display,win,&attribs);
			/*
			if(attribs.map_state == IsViewable)
				printf("IsViewable\n");
			if(attribs.map_state == IsUnmapped)
				printf("IsUnmapped\n");
			if(attribs.map_state == IsUnviewable)
				printf("IsUnviewable\n");
			*/
			/*
			printf(" !! dvsetwin:map_state %d\n",attribs.map_state);
			fflush(stdout);
			*/
		} while(attribs.map_state != IsViewable);
	}
	/*
	printf(" !! dvsetwin:end of loop\n");
	*/
	XSetInputFocus(display,fwin,RevertToParent,CurrentTime);
	XFlush(display);
}

#ifndef UNDERSCORE
void dvchin(iasc,nchar)
#else
void dvchin_(iasc,nchar)
#endif
int4		*iasc,*nchar;
{
	XEvent	        event;
	KeySym	        key;
	char		text[10];
	int		i;
	int		r;
#ifndef BSD
	struct termio	t,st;
#else
	struct sgttyb	t,st;
#endif	
	if(rmode) {
		for(i = 0; i < *nchar; i++)
			iasc[i] = ' ';
		return;
	}
	if(tmode) {
		toutpt(31);
		tbuff();
#ifndef BSD
		ioctl(0,TCGETA,&t);
		st = t;
		t.c_iflag &= ~(INLCR | IGNCR | ICRNL| ISTRIP);
		t.c_lflag &= ~(ICANON | ECHO);
		t.c_cc[VMIN] = 1;
		t.c_cc[VTIME] = 0;
		ioctl(0,TCSETAF,&t);
#else
		ioctl(0,TIOCGETP,&t);
		st = t;
		t.sg_flags |= CBREAK;
		t.sg_flags &= ~CRMOD;
		t.sg_flags &= ~ECHO;
		ioctl(0,TIOCSETN,&t);
#endif
		for(i = 0; i < *nchar; i++) {
			read(0,text,1);
			if(text[0] < ' ') {
				for(;i < *nchar;i++)
					iasc[i] = ' ';
				break;
			}
			iasc[i] = text[0];
		}
#ifndef	BSD
		ioctl(0,TCSETAW,&st);
#else
		ioctl(0,TIOCSETN,&st);
#endif
		igs = 0;
		return;
	}

	XSync(display,0);
	/*
	printf("dvchin:begin\n");
	fflush(stdout);
	*/
	dvsetwin(window,window);
	/*
	printf("dvchin:before select input\n");
	fflush(stdout);
	*/
	XSelectInput(display,window,KeyPressMask | KeymapStateMask);
	/*
	printf("dvchin:before xsync\n");
	fflush(stdout);
	*/
	i = 0;
	/*
	printf("dvchin:loop start\n");
	fflush(stdout);
	*/
	while (i < *nchar) {
		/*
		printf("dvchin:next event start\n");
		*/
		XNextEvent(display,&event);
		/*
		printf("dvchin:next event end\n");
		*/
		switch(event.type) {
		case MappingNotify:
			XRefreshKeyboardMapping((XMappingEvent *)&event);
			break;
		case KeyPress:
			r = XLookupString((XKeyEvent *)&event,text,10,&key,0);
			if(r == 1) {
				if(text[0] >= ' ' && text[0] <= '~')
					iasc[i++] = (int4)text[0];
				else {
					if(text[0] == 0x0D) {
						for(;i < *nchar;i++)
							iasc[i] = ' ';
						break;
					}
				}
			}
		}
	}
	XSelectInput(display,window,StructureNotifyMask);
	/*
	printf("dvchin:loop end\n");
	fflush(stdout);
	*/
}

#ifndef UNDERSCORE
void dvxyin(ix,iy)
#else
void dvxyin_(ix,iy)
#endif
int4		*ix,*iy;
{
	XEvent	event;
	int	done;
	int	ixh,ixl,iyh,iyl;
	static const char	tdata[] = {27,26};
#ifndef BSD
	struct termio	t,st;
#else
	struct sgttyb	t,st;
#endif	
	char    text[10];
		
	if(rmode) {
		*ix = 0;
		*iy = 0;
		return;
	}
	if(tmode) {
		toutst(2,tdata);
		tbuff();
#ifndef BSD
		ioctl(0,TCGETA,&t);
		st = t;
		t.c_iflag &= ~(INLCR | IGNCR | ICRNL| ISTRIP);
		t.c_lflag &= ~(ICANON | ECHO);
		t.c_cc[VMIN] = 1;
		t.c_cc[VTIME] = 0;
		ioctl(0,TCSETAF,&t);
#else
		ioctl(0,TIOCGETP,&t);
		st = t;
		t.sg_flags |= CBREAK;
		t.sg_flags &= ~CRMOD;
		t.sg_flags &= ~ECHO;
		ioctl(0,TIOCSETN,&t);
#endif
		read(0,text,1);
		read(0,text,1);
		ixh = text[0] & 0x1f;
		read(0,text,1);
		ixl = text[0] & 0x1f;
		read(0,text,1);
		iyh = text[0] & 0x1f;
		read(0,text,1);
		iyl = text[0] & 0x1f;
		if (tmode != 3) read(0,text,1);

#ifndef	BSD
		ioctl(0,TCSETAW,&st);
#else
		ioctl(0,TIOCSETN,&st);
#endif
		*ix = ((ixh << 5) + ixl) << 5;
		*iy = ((iyh << 5) + iyl) << 5;
		igs = 0;
		return;
	}

	dvsetwin(window,window);

	XDefineCursor(display,window,curs);
	XSelectInput(display,window,ButtonPressMask);
	XSync(display,0);

	done = 0;
	while (done == 0) {
		XNextEvent(display,&event);
		switch(event.type) {
		case ButtonPress:
			*ix = event.xbutton.x / delta;
			*iy = (ymax - event.xbutton.y) / delta;
			done = 1;
		}
	}
	XSelectInput(display,window,StructureNotifyMask);
	XUndefineCursor(display,window);
}

#ifndef UNDERSCORE
void dvsetv(id)
#else
void dvsetv_(id)
#endif
int4		*id;
{
	int		i;
	long		event_mask;

	if(rmode) return;
	if(tmode) return;

        i = *id;
	event_mask = 0;
	if((i & 1)) event_mask |= KeyPressMask;
	if((i & 2)) event_mask |= ButtonReleaseMask;
	if((i & 4)) event_mask |= ButtonPressMask;
	if((i & 8)) event_mask |= PointerMotionMask;
	if(event_mask != 0) {
		dvsetwin(window,window);
		XDefineCursor(display,window,curs);
		XSelectInput(display,window,event_mask);
		XSync(display,0);
	} else {
		XSelectInput(display,window,StructureNotifyMask);
		XUndefineCursor(display,window);
	}
}

#ifndef UNDERSCORE
void dvgetv(id,ix,iy,kd,kid)
#else
void dvgetv_(id,ix,iy,kd,kid)
#endif
int4		*id,*ix,*iy,*kd,*kid;
{
	XEvent	event;
	KeySym	        key;
	char		text[10];
	int		r;
	unsigned int	kidx;
		
	*id = -1;
	if(rmode) return;
	if(tmode) return;

	*id = 0;
	while(*id == 0) {
		XNextEvent(display,&event);
		switch(event.type) {
		case MappingNotify:
			XRefreshKeyboardMapping((XMappingEvent *)&event);
			*id = 0;
			break;
		case KeyPress:
			*ix = event.xkey.x / delta;
			*iy = (ymax - event.xkey.y) / delta;
			r = XLookupString((XKeyEvent *)&event,text,10,&key,0);
			if(r == 1)  {
				if(IsModifierKey(key))
					*id = 0;
				else {
					kidx = (unsigned char)text[0];
					*kid = (int4)kidx;
					*kd = event.xkey.keycode;
					*id = 1;
				}
			} else
				*id = 0;
			break;
		case ButtonRelease:
			*ix = event.xbutton.x / delta;
			*iy = (ymax - event.xbutton.y) / delta;
			*kid = event.xbutton.button;
			*id = 2;
			break;
		case ButtonPress:
			*ix = event.xbutton.x / delta;
			*iy = (ymax - event.xbutton.y) / delta;
			*kid = event.xbutton.button;
			*id = 4;
			break;
		case MotionNotify:
			*ix = event.xmotion.x / delta;
			*iy = (ymax - event.xmotion.y) / delta;
			*kid = 0;
			*id =  8;
			break;
		default:
			*id = 0;
			break;
		}
	}
}

#ifndef UNDERSCORE
void dveras()
#else
void dveras_()
#endif
{
	static const char	tdata[] = {27,12};

	if(rmode) return;
	if(tmode) {
		toutst(2,tdata);
		igs = 0;
		return;
	}
	XClearWindow(display,window);
}

#ifndef UNDERSCORE
void dvprnt()
#else
void dvprnt_()
#endif
{
	static const char	tdata[] = {27,23};

	if(rmode) return;
	if(tmode) {
		toutst(2,tdata);
		igs = 0;
		return;
	}
}

#ifndef UNDERSCORE
void dvbell()
#else
void dvbell_()
#endif
{
	if(rmode) return;
	if(tmode) {
		toutpt(7);
		igs = 0;
		return;
	}
	XBell(display,0);
	XFlush(display);
}

#ifndef UNDERSCORE
void dvsync()
#else
void dvsync_()
#endif
{
	if(rmode) return;
	if(tmode) return;
	XSync(display,0);
}

void dvline(ix,iy,imv)
int4	*ix,*iy;
int	imv;
{
	char	kgs[5];
	int	ixx,iyy,ix1,ix2,iy1,iy2,ixy,i;
	
	if(!imv && !igs) {
		toutpt(29);
		kgs[0]=0x20 | iy1p;
		if(ihr) {
			kgs[1]=0x60 | ixyp;
			kgs[2]=0x60 | iy2p;
			kgs[3]=0x20 | ix1p;
			kgs[4]=0x40 | ix2p;
			toutst(5,kgs);
		} else {
			kgs[1]=0x60 | iy2p;
			kgs[2]=0x20 | ix1p;
			kgs[3]=0x40 | ix2p;
			toutst(4,kgs);
		}
	}
	ixx = *ix;
	iyy = *iy;
	ix1 = ixx >> 10;
	ix2 = (ixx >> 5) & 0x1f;
	ixx = (ixx >> 3) & 0x3;
	iy1 = iyy >> 10;
	iy2 = (iyy >> 5) & 0x1f;
	iyy = (iyy >> 3) & 0x3;
	if(ihr)
		ixy = (iyy << 2) | ixx;
	else
		ixy = 0;

	if(imv) toutpt(29);
	
	i = 0;
	if(iy1 != iy1p)
		kgs[i++] = 0x20 | iy1;
	if((ixy != ixyp) || (iy2 != iy2p) || (ix1 != ix1p)) {
		if(ixy != ixyp)
			kgs[i++] = 0x60 | ixy;
		kgs[i++] = 0x60 | iy2;
	}
	if(ix1 != ix1p)
		kgs[i++] = 0x20 | ix1;
	kgs[i++] = 0x40 | ix2;
	toutst(i,kgs);
	
	ix1p = ix1;
	ix2p = ix2;
	iy1p = iy1;
	iy2p = iy2;
	ixyp = ixy;
	igs = 1;
}

#ifndef UNDERSCORE
void dvmove(ix,iy)
#else
void dvmove_(ix,iy)
#endif
int4		*ix,*iy;
{
	if(rmode) return;
	if(tmode) {
		dvline(ix,iy,1);
		return;
	}
	xpos = *ix * delta;
	ypos = ymax - *iy * delta;
}

#ifndef UNDERSCORE
void dvdraw(ix,iy)
#else
void dvdraw_(ix,iy)
#endif
int4		*ix,*iy;
{
	int		x,y;
	
	if(rmode) return;
	if(tmode) {
		dvline(ix,iy,0);
		return;
	}
	x = *ix * delta;
	y = ymax - *iy * delta;
	XDrawLine(display,window,gc,xpos,ypos,x,y);
	xpos = x;
	ypos = y;
}

#ifndef UNDERSCORE
void dvlins(ixn,iyn,np)
#else
void dvlins_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4            *np;
{
	int		i,n;
        int4            ix,iy;
	
	if(rmode | tmode) {  
		ix = ixn[0];
		iy = iyn[0];
#ifndef UNDERSCORE
		dvmove(&ix,&iy);
#else
		dvmove_(&ix,&iy);
#endif
		for (i = 1; i < *np; i++) {
			ix = ixn[i];
			iy = iyn[i];
#ifndef UNDERSCORE
			dvdraw(&ix,&iy);
#else
			dvdraw_(&ix,&iy);
#endif
		}
		return;
	}

        n = *np;
	if (n > NPNTMAX)
		n = NPNTMAX;

        for (i = 0; i < n; i++) {
		points[i].x = ixn[i] * delta;
		points[i].y = ymax - iyn[i] * delta;
	}
	XDrawLines(display,window,gc,points,n,CoordModeOrigin);
	xpos = points[n-1].x;
	ypos = points[n-1].y;
}

#ifndef UNDERSCORE
void dvpoly(ixn,iyn,np)
#else
void dvpoly_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4            *np;
{
	int		i,n;
        int4            ix,iy;
	
	if(rmode | tmode) {  
		ix = ixn[0];
		iy = iyn[0];
#ifndef UNDERSCORE
		dvmove(&ix,&iy);
#else
		dvmove_(&ix,&iy);
#endif
		for (i = 1; i < *np; i++) {
			ix = ixn[i];
			iy = iyn[i];
#ifndef UNDERSCORE
			dvdraw(&ix,&iy);
#else
			dvdraw_(&ix,&iy);
#endif
		}
		return;
	}

        n = *np;
	if (n > NPNTMAX)
		n = NPNTMAX;

	for (i = 0; i < n; i++) {
		points[i].x = ixn[i] * delta;
		points[i].y = ymax - iyn[i] * delta;
	}
	XFillPolygon(display,window,gc,points,n,Complex,CoordModeOrigin);
	XDrawLines(display,window,gc,points,n,CoordModeOrigin);
	xpos = points[n-1].x;
	ypos = points[n-1].y;
}

#ifndef UNDERSCORE
void dvtype(ich)
#else
void dvtype_(ich)
#endif
int4		*ich;
{
	char *str;

	printf(" # Welcome to GSAF \n");
	str = getenv("GSGDP");
	if(str)
		*ich=0;
	else
		*ich=1;
}

#ifndef UNDERSCORE
void dvopen(ich)
#else
void dvopen_(ich)
#endif
int4		*ich;
{
	unsigned long	valuemask;
	XSetWindowAttributes attributes;
	Visual		*v;
	unsigned	width;
	unsigned	height;
	int		x = 100, y = 50;
	unsigned	border_width = 2;
	XSizeHints	size_hints;
	XWMHints	wmhints;
	int		revert_to;
	static const char	tdata1[] = {27,90,27,68,46,27,75,65,
		                            27,76,48,27,77,66,27,89};
	char *str;
	int c1,c2;

	c1 = *ich;
	c2 = 1;
	chkeymode = 1;

        cgamma = 1.0;
	str = getenv("GSGAMMA");
	if(str)
		cgamma = atof(str);

	str = getenv("GSGDP");
	if(str) {
		c1 = str[0];
		if(c1 == '-'){
			chkeymode = 0;
			if(str[1]) {
				c1 = str[1];
				if(str[2])
					c2 = str[2];
			}
		} else {
			if(str[1])
				c2 = str[1];
		}
	}
	*ich = c2;

	switch(c1) {
	case '0' : rmode = 1; *ich = - *ich; return;
	case '1' : width =  512; height =  380; break;
	case '2' : width =  640; height =  475; break;
	case '3' : width =  768; height =  570; break;
	case '4' : width =  896; height =  665; break;
	case '5' : width = 1024; height =  760; break;
	case '6' : width = 1280; height =  950; break;
	case '7' : tmode = tmode+1;
	case '8' : tmode = tmode+1;
	case '9' : tmode = tmode+1;
		igs = 0;
		ihr = 0;
		ix1p = -1;
		ix2p = -1;
		iy1p = -1;
		iy2p = -1;
		ixyp = 0;
		tinit();
		if(tmode == 1) toutst(16,tdata1);
		return;
	default  : width =  512; height = 380;
	}
	if((display = XOpenDisplay(NULL)) == NULL) {
		fprintf(stderr,"Can't open %s\n",XDisplayName(NULL));
		rmode = 1;
		return;
	}
	
	v = DefaultVisual(display, DefaultScreen(display));
	switch(v->class) {
	case StaticGray  : gray = 1; fullcolor = 0; break;
	case GrayScale   : gray = 1; fullcolor = 0; break;
	case StaticColor : gray = 0; fullcolor = 0; break;
	case PseudoColor : gray = 0; fullcolor = 0; break;
	case TrueColor   : gray = 0; fullcolor = 1; break;
	case DirectColor : gray = 0; fullcolor = 1; break;
	}
	
	screen = DefaultScreen(display);
	cm = DefaultColormap(display,screen);

	rootw = RootWindow(display,screen);
	XGetInputFocus(display,&focus,&revert_to);
	/*
        printf(" !! focus= %d\n",focus);
	*/
	if (focus == PointerRoot) {
		/*
		XQueryPointer(display,rootw,&w1,&childw,&rootx,&rooty,
			      &winx,&winy,&keyb);
		parentw = childw;
		printf(" !! rootx,y= %d,%d\n",rootx,rooty);
		printf(" !! winx,y = %d,%d\n",winx,winy);
		XQueryPointer(display,parentw,&w1,&childw,&rootx,&rooty,
			      &winx,&winy,&keyb);
		printf(" !! w1,childw= %d,%d\n",w1,childw);
		printf(" !! rootx,y= %d,%d\n",rootx,rooty);
		printf(" !! winx,y = %d,%d\n",winx,winy);
		*/
		parentw = rootw;
		chkeymode = 0;
	} else
		parentw = focus;
        /*
	printf(" !! parentw= %d\n",parentw);
	*/

	window = XCreateSimpleWindow(display,
				     RootWindow(display,screen),
				     x,y,width,height,border_width,
				     BlackPixel(display,screen),
				     WhitePixel(display,screen));

	if(WhitePixel(display,screen) == 0)
		whitezero = 1;
	else
		whitezero = 0;
	/*
	attributes.backing_store = WhenMapped;
	*/
	attributes.backing_store = Always;
	valuemask = CWBackingStore;
	XChangeWindowAttributes(display,window,valuemask,&attributes);

	wmhints.input = True;
	wmhints.flags = InputHint;
	XSetWMHints(display,window,&wmhints);
	
	size_hints.flags = USPosition | USSize;
	size_hints.x = x;
	size_hints.y = y;
	size_hints.width = width;
	size_hints.height = height;
	XSetWMNormalHints(display,window,&size_hints);
	
	XStoreName(display,window,"GSAF");
	
	curs = XCreateFontCursor(display,XC_crosshair);
	gc = XCreateGC(display,window,0,0);

	XSelectInput(display,window,StructureNotifyMask);
	
	delta = (float)width/(1024*32);
	ymax = height + 1;

	mapped = 0;
	icls = -1;
        ibls = -1;
	ilns = -1;
        ndcol = 0;
		
	rmode = 0;
}

#ifndef UNDERSCORE
void dvclos(ich)
#else
void dvclos_(ich)
#endif
int4		*ich;
{
		*ich = 1;
	if(rmode) return;
	if(tmode) return;

	XFreeGC(display,gc);
	XFreeCursor(display,curs);
	XDestroyWindow(display,window);
	XCloseDisplay(display);
	*ich = 1;
}

#ifndef UNDERSCORE
void dvoptn(kopt,iopt)
#else
void dvoptn_(kopt,iopt)
#endif
char		*kopt;
int4		*iopt;
{
}

#ifndef UNDERSCORE
void dvgrmd()
#else
void dvgrmd_()
#endif
{
	static const char	tdata1[] = {27,90,27,75,66,27,77,65};
	static const char	tdata2[] = {27,'%','!','8'};
	static const char	tdata3[] = {27,'[','?','3','8','h'};
	
	if(rmode) return;
	if(tmode) {
		if(tmode == 1) toutst(8,tdata1);
		if(tmode == 2) toutst(4,tdata2);
		if(tmode == 3) toutst(6,tdata3);
		igs = 0;
		return;
	}	
	dvsetwin(window,window);
}

#ifndef UNDERSCORE
void dvchmd()
#else
void dvchmd_()
#endif
{
	static const char	tdata1[] = {27,75,65,27,77,66,27,89};
	static const char	tdata2[] = {27,'2'};
	static const char	tdata3[] = {27,3};

	if(rmode) return;
	if(tmode) {
		if(tmode == 1) toutst(8,tdata1);
		if(tmode == 2) toutst(2,tdata2);
		if(tmode == 3) toutst(2,tdata3);
		tbuff();
		igs = 0;
		return;
	}
	dvsetwin(parentw,focus);
}

#ifndef UNDERSCORE
void dvpags(npage,sizex,sizey,lkeep)
#else
void dvpags_(npage,sizex,sizey,lkeep)
#endif
int4		*npage;
float		*sizex;
float		*sizey;
int4		*lkeep;
{
  Window root;
  int x,y;
  unsigned int width,height,border_width,depth;
  float ratio;
  Status status;
  int i;

	if(rmode) return;
	ilns = -1;
	ibls = -1;
	icls = -1;

        if(!tmode) {
		for (i=0; i < ndcol; i++)
			dpixel[i] = dcol[i].pixel;
		XFreeColors(display,cm,dpixel,ndcol,0);
		nccol = 0;
		ndcol = 0;
	}

#ifndef UNDERSCORE
	dvgrmd();
#else
	dvgrmd_();
#endif
	if(!tmode) {
		status = XGetGeometry(display,window,&root,&x,&y,&width,&height,
				      &border_width,&depth);
		ratio = height / width;
		if (ratio >= 760.0/1024.0)
			delta = (float)width/(1024*32);
		else
			delta = (float)height/(760*32);
		ymax = height + 1;
	}

	pageno = *npage;
#ifndef UNDERSCORE
	if(!*lkeep) dveras();
#else
	if(!*lkeep) dveras_();
#endif
}

void dupwin()
{
	char	cmd[80],s[20];

	sprintf(s,"gsdump%d",pageno);
	XStoreName(display,window,s);
	XFlush(display);
	sprintf(cmd,"xwd -name %s -out %s",s,s);
	system(cmd);
	sprintf(cmd,"xwud -noclick -in %s &",s);
	system(cmd);
	XStoreName(display,window,"GSAF");
}


#ifndef UNDERSCORE
void dvpage(ich)
#else
void dvpage_(ich)
#endif
int4		*ich;
{
	int4		ix,iy,ichar;
	char		s[20];
	
	if(rmode) {
		*ich = 0;
		return;
	}
	if(chkeymode){
		if(*ich != 0) {
			ix = 32000;
			iy = 10;
			ichar = 1;
#ifndef UNDERSCORE
			dvmove(&ix,&iy);
			dvchin(ich,&ichar);
#else
			dvmove_(&ix,&iy);
			dvchin_(ich,&ichar);
#endif
		}
	} else {
		if(!tmode) {
			fflush(stdout);
			/*
			XSetInputFocus(display,focus,RevertToParent,CurrentTime);
			XMapRaised(display,window);
			XFlush(display);
			*/
			dvsetwin(window,focus);
		}
		if(*ich != 0) {
			fgets(s,20,stdin);
			if(s[0])
				*ich = (int4)s[0];
			else
				*ich = 32;
		}
	}
	if(!tmode)
		if((ich[0] == 'd') || (ich[0] == 'D'))
			dupwin();
#ifndef UNDERSCORE
	dvchmd();
#else
	dvchmd_();
#endif
}

#ifndef UNDERSCORE
void dvgrps()
#else
void dvgrps_()
#endif
{
}

#ifndef UNDERSCORE
void dvgrpe()
#else
void dvgrpe_()
#endif
{
}

#ifndef UNDERSCORE
void dvtext(ix,iy,iasc,nchar)
#else
void dvtext_(ix,iy,iasc,nchar)
#endif
int4		*ix,*iy,*iasc,*nchar;
{
	int		i,x,y,n;
	char		s[256];
	
	if(rmode) return;

	n = *nchar;
	for(i=0; i<n; i++)
		s[i] = (char)iasc[i];
	s[n] = (char)0;

	if(tmode) {
#ifndef UNDERSCORE
		dvmove(ix,iy);
#else
		dvmove_(ix,iy);
#endif
		toutpt(31);
		toutst(n,s);
		igs = 0;
		return;
	}
	
	x = *ix * delta;
	y = ymax - *iy * delta;
	XDrawString(display,window,gc,x,y,s,*nchar);
	xpos = x + n * dxch;
	ypos = y + n * dych;
}

void dvsetdcol(c)
XColor *c;
{
	int n;

	for (n = 0; n < ndcol; n++) {    
		if (dcol[n].red == c->red &&
		    dcol[n].green == c->green &&
		    dcol[n].blue == c->blue) {
			c->pixel = dcol[n].pixel;
			return;
		}
	}

	if (XAllocColor(display,cm,c)) {
		if (ndcol < DCOLSIZE) {
			dcol[ndcol] = *c;
			++ndcol;
		}
	}else
		c->pixel = dcol[0].pixel;
}

void dvsetccol(c)
XColor *c;
{
	int n;
  
	for (n=0; n < nccol && 
	     (ccol[n].red != c->red || 
	      ccol[n].green != c->green || 
	      ccol[n].blue != c->blue); n++);

	if (n == nccol) {
		dvsetdcol(c);
		if (nccol < CCOLSIZE)
			++nccol;
		else
			--n;
	} else
		c->pixel = ccol[n].pixel;

	for (; n > 0; --n)
		ccol[n] = ccol[n-1];
	ccol[0] = *c;
}

int cconv(i)
int i;
{
	double r,rr;
	int ii;

	if (abs(cgamma -1.0) <= 0.01 || cgamma <= 0.0)
		return i*256;
	else{
		r = i / 255.0;
		rr = pow(r,1.0/cgamma);
		ii = rr * (256*255);
		return ii;
	}
}

void dvcrgbx(ir,ig,ib)
int4		ir,ig,ib;
{
	XColor		c;
		
	c.red   = cconv(ir);
       	c.green = cconv(ig);
	c.blue  = cconv(ib);
	if(fullcolor == 1)
		XAllocColor(display,cm,&c);
	else
		dvsetccol(&c);
	XSetForeground(display,gc,c.pixel);
}

#ifndef UNDERSCORE
void dvstln(iln,ibl,icl)
#else
void dvstln_(iln,ibl,icl)
#endif
int4		*iln,*ibl,*icl;
{
	unsigned int	line_width;
	/*	int			cap_style = CapButt;*/
	int			cap_style = CapProjecting;
	int			join_style = JoinMiter;
	static const char	dash_list1[] = { 2, 6 };
	static const char	dash_list2[] = { 5, 3 };
	static const char	dash_list3[] = { 12, 4 };
	static const char	dash_list4[] = { 10, 2, 2, 2};
	static const char	dash_list5[] = { 17, 2, 3, 2};
	static const char	dash_list6[] = { 6, 2, 2, 2, 2, 2};
	static const char	dash_list7[] = { 12, 2, 3, 2, 3, 2};
	char			tdata[3];
		
	if(rmode) return;
	if(tmode) {
		if(*iln != -1 && *iln != ilns) {
			tdata[0] = 27;
			switch(tmode) { 
			case 1: 
				switch(*iln) {
				case 0 : tdata[1] = 96; break;
				case 1 : tdata[1] = 97; break;
				case 2 : tdata[1] = 99; break;
				case 3 : tdata[1] =100; break;
				case 4 : tdata[1] = 98; break;
				case 5 : tdata[1] = 98; break;
				case 6 : tdata[1] =101; break;
				case 7 : tdata[1] =101; break;
				default : tdata[1] = 96; break;
				}
				break;
			case 2: 
				switch(*iln) {
				case 0 : tdata[1] = 96; break;
				case 1 : tdata[1] = 97; break;
				case 2 : tdata[1] = 99; break;
				case 3 : tdata[1] =100; break;
				case 4 : tdata[1] = 98; break;
				case 5 : tdata[1] =102; break;
				case 6 : tdata[1] =101; break;
				case 7 : tdata[1] =103; break;
				default : tdata[1] = 96; break;
				}
				break;
			case 3:
				switch(*iln) {
				case 0 : tdata[1] = 96; break;
				case 1 : tdata[1] = 97; break;
				case 2 : tdata[1] = 99; break;
				case 3 : tdata[1] = 99; break;
				case 4 : tdata[1] = 98; break;
				case 5 : tdata[1] = 98; break;
				case 6 : tdata[1] = 98; break;
				case 7 : tdata[1] = 98; break;
				default : tdata[1] = 96; break;
				}
				break;
			}
			toutst(2,tdata);
			ilns = *iln;
		}
		if(*ibl != -1 && *ibl != ibls && tmode == 1) {
			tdata[0] = 27;
			switch(*ibl) {
			case 0 : tdata[1] = 48; break;
			case 1 : tdata[1] = 57; break;
			case 2 : tdata[1] = 58; break;
			case 3 : tdata[1] = 59; break;
			case 4 : tdata[1] = 60; break;
			default : tdata[1] = 48; break;
			}
			toutst(2,tdata);
			ibls = *ibl;
		}
		if(*icl != -1 && *icl != icls && tmode == 1) {
			tdata[0] = 27;
			tdata[1] = 67;
			switch(*icl) {
			case 0 : tdata[2] = 64; break;
			case 1 : tdata[2] = 68; break;
			case 2 : tdata[2] = 65; break;
			case 3 : tdata[2] = 69; break;
			case 4 : tdata[2] = 66; break;
			case 5 : tdata[2] = 70; break;
			case 6 : tdata[2] = 67; break;
			case 7 : tdata[2] = 71; break;
			default : tdata[1] = 71; break;
			}
			toutst(3,tdata);
			icls = *icl;
		}
		igs = 0;
		return;
	}
/*
	line_width = *ibl;
*/
	line_width = 0;
	if(*iln != -1 && *iln != ilns) {
		switch(*iln) {
		case 0: XSetLineAttributes(display,gc,line_width,LineSolid,
					   cap_style,join_style);
			break;
		case 1: XSetDashes(display,gc,0,dash_list1,2);
			XSetLineAttributes(display,gc,line_width,LineOnOffDash,
					   cap_style,join_style);
			break;
		case 2: XSetDashes(display,gc,0,dash_list2,2);
			XSetLineAttributes(display,gc,line_width,LineOnOffDash,
					   cap_style,join_style);
			break;
		case 3: XSetDashes(display,gc,0,dash_list3,2);
			XSetLineAttributes(display,gc,line_width,LineOnOffDash,
					   cap_style,join_style);
			break;
		case 4: XSetDashes(display,gc,0,dash_list4,4);
			XSetLineAttributes(display,gc,line_width,LineOnOffDash,
					   cap_style,join_style);
			break;
		case 5: XSetDashes(display,gc,0,dash_list5,4);
			XSetLineAttributes(display,gc,line_width,LineOnOffDash,
					   cap_style,join_style);
			break;
		case 6: XSetDashes(display,gc,0,dash_list6,6);
			XSetLineAttributes(display,gc,line_width,LineOnOffDash,
					   cap_style,join_style);
			break;
		case 7: XSetDashes(display,gc,0,dash_list7,6);
			XSetLineAttributes(display,gc,line_width,LineOnOffDash,
					   cap_style,join_style);
			break;
		}
		ilns = *iln;
	}
	ibls = *ibl;

	if(*icl != -1) {
		if(gray) {
		switch(*icl) {
		case 7: dvcrgbx(0,0,0);
			break;
		case 6: dvcrgbx(160,160,160);
			break;
		case 5: dvcrgbx(176,176,176);
			break;
		case 4: dvcrgbx(192,192,192);
			break;
		case 3: dvcrgbx(208,208,208);
			break;
		case 2: dvcrgbx(224,224,224);
			break;
		case 1: dvcrgbx(240,240,240);
			break;
		case 0: dvcrgbx(255,255,255);
			break;
		}
		} else {
			switch(*icl) {
			case 7: dvcrgbx(0,0,0);
				break;
			case 5: dvcrgbx(0,0,255);
				break;
			case 6: dvcrgbx(255,0,0);
				break;
			case 3: dvcrgbx(255,0,255);
				break;
			case 4: dvcrgbx(0,195,0);
				break;
			case 1: dvcrgbx(0,215,234);
				break;
			case 2: dvcrgbx(234,215,0);
				break;
			case 0: dvcrgbx(255,255,255);
				break;
			}
		}
		icls = *icl;
	}
}

#ifndef UNDERSCORE
void dvlwdt(iw)
#else
void dvlwdt_(iw)
#endif
int4		*iw;
{
        unsigned int	        line_width;
	/*	int			cap_style = CapButt;*/
	int			cap_style = CapProjecting;
	int			join_style = JoinMiter;
	char			tdata[3];

        line_width = *iw * delta;
        ibls = -2;

	if(rmode) return;
	if(tmode) {
		if(tmode == 1) {
			tdata[0] = 27;
			switch(*iw) {
			case 0 : tdata[1] = 48; break;
			case 1 : tdata[1] = 57; break;
			case 2 : tdata[1] = 58; break;
			case 3 : tdata[1] = 59; break;
			case 4 : tdata[1] = 60; break;
			default : tdata[1] = 48; break;
			}
			toutst(2,tdata);
		}
		igs = 0;
		return;
	}

	XSetLineAttributes(display,gc,line_width,LineSolid,
			   cap_style,join_style);
}

#ifndef UNDERSCORE
void dvcrgb(ir,ig,ib)
#else
void dvcrgb_(ir,ig,ib)
#endif
int4		*ir,*ig,*ib;
{
        icls = -2;
	if(rmode) return;
	if(tmode) return;

	dvcrgbx(*ir,*ig,*ib);
}

#ifndef UNDERSCORE
void dvgcfunc(id)
#else
void dvgcfunc_(id)
#endif
int4		*id;
{
  	int     func;
	static const int	ifunc[] = {15,7,11,3,13,5,9,1,14,6,10,2,12,4,8,0};

	if(rmode) return;
	if(tmode) return;
        if((*id < 0) || (*id >15)) return;

	if(whitezero)
		func = *id;
	else
		func = ifunc[*id];
	XSetFunction(display,gc,func);
}  

/*
#ifndef UNDERSCORE
void dvinfo(icells,iplanes,idepth,iwhite,iblack)
#else
void dvinfo_(icells,iplanes,idepth,iwhite,iblack)
#endif
int4		*icells,*iplanes,*idepth,*iwhite,*iblack;
{
  	int       cells,planes,depth,white,black;

	cells = XDisplayCells(display,screen);
	planes = XDisplayPlanes(display,screen);
	depth = XDefaultDepth(display,screen);
	white = XWhitePixel(display,screen);
	black = XBlackPixel(display,screen);
	*icells = cells;
	*iplanes = planes;
	*idepth = depth;
	*iwhite = white;
	*iblack = black;
}  
*/

#ifndef UNDERSCORE
void dvstch(ichh,ichw,ichsp,angl,tilt,ind)
#else
void dvstch_(ichh,ichw,ichsp,angl,tilt,ind)
#endif
int4		*ichh,*ichw,*ichsp,*ind;
float		*angl,*tilt;
{
	*ind = 0;
	if(rmode) return;
	*ind = 1;
	if(tmode) return;
	dxch =   *ichsp * cos(*angl * DEG) * delta;
	dych = - *ichsp * sin(*angl * DEG) * delta;
}

#ifndef UNDERSCORE
void dvfont(ifnt,ind)
#else
void dvfont_(ifnt,ind)
#endif
int4		*ifnt,*ind;
{
	*ind = 1;
}


#ifndef UNDERSCORE
void dvrgbtrg(ixn,iyn,ir,ig,ib)
#else
void dvrgbtrg_(ixn,iyn,ir,ig,ib)
#endif
int4           ixn[],iyn[];
int4           ir[],ig[],ib[];
{
	int		i;
        int4            ix,iy;
	int4            red,green,blue;
	

	if(rmode | tmode) {  
		ix = ixn[0];
		iy = iyn[0];
#ifndef UNDERSCORE
		dvmove(&ix,&iy);
#else
		dvmove_(&ix,&iy);
#endif
		for (i = 1; i < 3; i++) {
			ix = ixn[i];
			iy = iyn[i];
#ifndef UNDERSCORE
			dvdraw(&ix,&iy);
#else
			dvdraw_(&ix,&iy);
#endif
		}
		return;
	}

	red   = (ir[0]+ir[1]+ir[2])/3;
	green = (ig[0]+ig[1]+ig[2])/3;
	blue  = (ib[0]+ib[1]+ib[2])/3;
	dvcrgbx(red,green,blue);

        for (i = 0; i < 3; i++) {
		points[i].x = ixn[i] * delta;
		points[i].y = ymax -iyn[i] * delta;
	}
	points[3].x = ixn[0] * delta;
	points[3].y = ymax -iyn[0] * delta;

	XFillPolygon(display,window,gc,points,3,Convex,CoordModeOrigin);
	XDrawLines(display,window,gc,points,4,CoordModeOrigin);
	xpos = points[2].x;
	ypos = points[2].y;
}

/*
main()
{
	int4		ich,ix,iy,iasc[8],lkeep,nchar,ichh,ichw,ichsp;
	int4		iln,ibl,icl;
	int		i;
	float		angl,tilt;
	
	dvopen(&ich);
	lkeep = 0;
	dvpags(&lkeep);
	dvpage(&ich);
	
	dvpags(&lkeep);
	ichh = 24*32;
	ichw = 16*32;
	ichsp = 24*32;
	angl = 0.0;
	tilt = 0.0;
	dvstch(&ichh,&ichw,&ichsp,&angl,&tilt,&ich);
	for(i=0; i<8; i++) {
		iln = i;
		ibl = i;
		icl = i;
		dvstln(&iln,&ibl,&icl);
		ix = 64;
		iy = 32*32 + 64*32 * i;
		dvmove(&ix,&iy);
		ix = 1024*32-64;
		dvdraw(&ix,&iy);
	}
	iln = 0;
	ibl = 0;
	icl = 7;
	dvstln(&iln,&ibl,&icl);
	ix = 64;
	iy = 64;
	dvmove(&ix,&iy);
	ix = 1024*32-64;
	iy = 780*32-64;
	dvdraw(&ix,&iy);
	dvxyin(&ix,&iy);
	dvdraw(&ix,&iy);

	nchar = 4;
	dvchin(&iasc,&nchar);

	dvstln(&iln,&ibl,&icl);

	for(i=1;i<5;i++) {
		ichh = 24*32*i;
		ichw = 16*32*i;
		ichsp = 24*32*i;
		angl = 90.0*(i-1);
		dvstch(&ichh,&angl,&tilt,&ich);
		ix = 128*32*i;
		iy = 512*32;
		dvtext(&ix,&iy,&iasc,&nchar);
	}
	
	dvpage(&ich);
	dvclos();
}
*/

