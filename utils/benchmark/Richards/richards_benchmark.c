
/*  C version of the systems programming language benchmark 
**  Author:  M. J. Jordan  Cambridge Computer Laboratory. 
**  
**  Modified by:  M. Richards, Nov 1996
**    to be ANSI C and runnable on 64 bit machines + other minor changes
**  Modified by:  M. Richards, 20 Oct 1998
**    made minor corrections to improve ANSI compliance (suggested
**    by David Levine)
**
**  Compile with, say
**
**  gcc -o bench bench.c
**
**  or
**
**  gcc -o bench100 -Dbench100 bench.c  (for a version that obeys
**                                       the main loop 100x more often)
*/

#include <stdio.h>
#include <stdlib.h>

#ifdef NDEBUG
#define DEBUG(...)
#define DEBUG_DO(x)
#else
#define DEBUG(...) printf(__VA_ARGS__)
#define DEBUG_DO(x) {x;}
#endif

#ifdef SMALL_PROBLEM_SIZE
#define                Count           1000*1000
#define                Qpktcountval    2326410
#define                Holdcountval     930563
#else
#define                Count           10000*1000
#define                Qpktcountval    23263894
#define                Holdcountval     9305557
#endif


#define                TRUE            1
#define                FALSE           0
#define                MAXINT          32767

#define                BUFSIZE         3
#define                I_IDLE          1
#define                I_WORK          2
#define                I_HANDLERA      3
#define                I_HANDLERB      4
#define                I_DEVA          5
#define                I_DEVB          6
#define                PKTBIT          1
#define                WAITBIT         2
#define                HOLDBIT         4
#define                NOTPKTBIT       !1
#define                NOTWAITBIT      !2
#define                NOTHOLDBIT      0XFFFB

#define                S_RUN           0
#define                S_RUNPKT        1
#define                S_WAIT          2
#define                S_WAITPKT       3
#define                S_HOLD          4
#define                S_HOLDPKT       5
#define                S_HOLDWAIT      6
#define                S_HOLDWAITPKT   7

#define                K_DEV           1000
#define                K_WORK          1001

struct packet
{
    struct packet  *p_link;
    int             p_id;
    int             p_kind;
    int             p_a1;
    char            p_a2[4];
};

void printpkt(struct packet *pkt) {
  printf("PACKET\n");
  if (pkt->p_link) {
    printf("link %d\n", pkt->p_link->p_id);
  }
  printf("id %d\n", pkt->p_id);
  printf("a1 %d\n", pkt->p_a1);
  printf("a2 ");
  for (int i = 0; i <= BUFSIZE; ++i) {
    printf("%d ", pkt->p_a2[i]);
  }
  printf("\n");
}

struct task
{
    struct task    *t_link;
    int             t_id;
    int             t_pri;
    struct packet  *t_wkq;
    int             t_state;
    struct task    *(*t_fn)(struct packet *);
    long            t_v1;
    long            t_v2;
};

char  alphabet[28] = "0ABCDEFGHIJKLMNOPQRSTUVWXYZ";
struct task *tasktab[11]  =  {0,0,0,0,0,0,0,0,0,0,0};
struct task *tasklist    =  0;
struct task *tcb;
long    taskid;
long    v1;
long    v2;
int     qpktcount    =  0;
int     holdcount    =  0;
int     tracing      =  1;
int     layout       =  0;

void append(struct packet *pkt, struct packet *ptr);

void createtask(int id,
                int pri,
                struct packet *wkq,
                int state,
                struct task *(*fn)(struct packet *),
                long v1,
                long v2)
{
    struct task *t = (struct task *)malloc(sizeof(struct task));

    tasktab[id] = t;
    t->t_link   = tasklist;
    t->t_id     = id;
    t->t_pri    = pri;
    t->t_wkq    = wkq;
    t->t_state  = state;
    t->t_fn     = fn;
    t->t_v1     = v1;
    t->t_v2     = v2;
    tasklist    = t;
}

struct packet *pkt(struct packet *link, int id, int kind)
{
    int i;
    struct packet *p = (struct packet *)malloc(sizeof(struct packet));

    for (i=0; i<=BUFSIZE; i++)
        p->p_a2[i] = 0;

    p->p_link = link;
    p->p_id = id;
    p->p_kind = kind;
    p->p_a1 = 0;

    return (p);
}

void trace(char a)
{
   if ( --layout <= 0 )
   {
        printf("\n");
        layout = 50;
    }

    printf("%c", a);
}

void schedule()
{
    while ( tcb != 0 )
    {
      DEBUG("TCB %d state %d\n", tcb->t_id, tcb->t_state);
      
        struct packet *pkt;
        struct task *newtcb;

        pkt=0;

        switch ( tcb->t_state )
        {
            case S_WAITPKT:
                pkt = tcb->t_wkq;
                tcb->t_wkq = pkt->p_link;
                tcb->t_state = tcb->t_wkq == 0 ? S_RUN : S_RUNPKT;

            case S_RUN:
            case S_RUNPKT:
                taskid = tcb->t_id;
                v1 = tcb->t_v1;
                v2 = tcb->t_v2;
                if (tracing==TRUE) trace(taskid+'0');

                newtcb = (*(tcb->t_fn))(pkt);
                tcb->t_v1 = v1;
                tcb->t_v2 = v2;
                tcb = newtcb;
                break;

            case S_WAIT:
            case S_HOLD:
            case S_HOLDPKT:
            case S_HOLDWAIT:
            case S_HOLDWAITPKT:
                tcb = tcb->t_link;
                break;

            default:
                return;
        }
    }
}

struct task *Wait(void)
{
    tcb->t_state |= WAITBIT;
    return (tcb);
}

struct task *holdself(void)
{
    ++holdcount;
    tcb->t_state |= HOLDBIT;
    return (tcb->t_link) ;
}

struct task *findtcb(int id)
{
    struct task *t = 0;

    if (1<=id && id<=(long)10)
    t = tasktab[id];
    if (t==0) printf("\nBad task id %d\n", id);
    return(t);
}

struct task *release(int id)
{
    struct task *t;

    t = findtcb(id);
    if ( t==0 ) return (0);

    t->t_state &= NOTHOLDBIT;
    if ( t->t_pri > tcb->t_pri ) return (t);

    return (tcb) ;
}


struct task *qpkt(struct packet *pkt)
{
    struct task *t;

    t = findtcb(pkt->p_id);
    if (t==0) return (t);

    qpktcount++;

    pkt->p_link = 0;
    pkt->p_id = taskid;

   if (t->t_wkq==0)
    {
        t->t_wkq = pkt;
        t->t_state |= PKTBIT;
        if (t->t_pri > tcb->t_pri) return (t);
    }
    else
    {
        append(pkt, (struct packet *)&(t->t_wkq));
    }

    return (tcb);
}

struct task *idlefn(struct packet *pkt)
{
  DEBUG("IDLE\n");
    --v2;
    if ( v2==0 ) return ( holdself() );

    if ( (v1&1) == 0 )
    {
        v1 = ( v1>>1) & MAXINT;
        return ( release(I_DEVA) );
    }
    else
    {
        v1 = ( (v1>>1) & MAXINT) ^ 0XD008;
        return ( release(I_DEVB) );
    }
}

struct task *workfn(struct packet *pkt)
{
  DEBUG("WORK\n");
    if ( pkt==0 ) return ( Wait() );
    else
    {
        int i;

        v1 = I_HANDLERA + I_HANDLERB - v1;
        pkt->p_id = v1;

        pkt->p_a1 = 0;
        for (i=0; i<=BUFSIZE; i++)
        {
            v2++;
            if ( v2 > 26 ) v2 = 1;
            (pkt->p_a2)[i] = alphabet[v2];
        }
        return ( qpkt(pkt) );
    }
}

struct task *handlerfn(struct packet *pkt)
{
    DEBUG("HANDLE\n");
    if ( pkt!=0) {
      DEBUG_DO(printpkt(pkt));
      append(pkt, (struct packet *)(pkt->p_kind==K_WORK ? &v1 : &v2));
    }
    if ( v1!=0 )
    {
        int count;
        struct packet *workpkt = (struct packet *)v1;
        count = workpkt->p_a1;

        if ( count > BUFSIZE )
        {
           v1 = (long)(((struct packet *)v1)->p_link);
           return ( qpkt(workpkt) );
        }

        if ( v2!=0 )
        {
            struct packet *devpkt;

            devpkt = (struct packet *)v2;
            v2 = (long)(((struct packet *)v2)->p_link);
            devpkt->p_a1 = workpkt->p_a2[count];
            workpkt->p_a1 = count+1;
            return( qpkt(devpkt) );
        }
    }
    return ( Wait() );
}

struct task *devfn(struct packet *pkt)
{
    DEBUG("DEV\n");
    if ( pkt==0 )
    {
        if ( v1==0 ) return ( Wait() );
        pkt = (struct packet *)v1;
        v1 = 0;
        return ( qpkt(pkt) );
    }
    else
    {
        v1 = (long)pkt;
        if (tracing==TRUE) trace(pkt->p_a1);
        return ( holdself() );
    }
}

void append(struct packet *pkt, struct packet *ptr)
{
    pkt->p_link = 0;

    while ( ptr->p_link ) ptr = ptr->p_link;

    ptr->p_link = pkt;
}

int main()
{
    struct packet *wkq = 0;
    int retval;

    printf("Bench mark starting\n");

    createtask(I_IDLE, 0, wkq, S_RUN, idlefn, 1, Count);

    wkq = pkt(0, 0, K_WORK);
    wkq = pkt(wkq, 0, K_WORK);

    createtask(I_WORK, 1000, wkq, S_WAITPKT, workfn, I_HANDLERA, 0);

    wkq = pkt(0, I_DEVA, K_DEV);
    wkq = pkt(wkq, I_DEVA, K_DEV);
    wkq = pkt(wkq, I_DEVA, K_DEV);

    createtask(I_HANDLERA, 2000, wkq, S_WAITPKT, handlerfn, 0, 0);

    wkq = pkt(0, I_DEVB, K_DEV);
    wkq = pkt(wkq, I_DEVB, K_DEV);
    wkq = pkt(wkq, I_DEVB, K_DEV);

    createtask(I_HANDLERB, 3000, wkq, S_WAITPKT, handlerfn, 0, 0);

    wkq = 0;
    createtask(I_DEVA, 4000, wkq, S_WAIT, devfn, 0, 0);
    createtask(I_DEVB, 5000, wkq, S_WAIT, devfn, 0, 0);

    tcb = tasklist;

    qpktcount = holdcount = 0;

    printf("Starting\n");

    tracing = FALSE;
    layout = 0;

    schedule();

    printf("finished\n");

    printf("qpkt count = %d  holdcount = %d\n",
           qpktcount, holdcount);

    printf("These results are ");
    if (qpktcount == Qpktcountval && holdcount == Holdcountval) {
      printf("correct");
      retval = 0;
    } else {
      printf("incorrect");
      retval = 1;
    }
    
    printf("\nend of run\n");
    return retval;
}

