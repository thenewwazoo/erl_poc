
#include <stdio.h>
#include <stdlib.h>
#include <string.h> // for memcpy
#include <poll.h>
#include <fcntl.h>

#include "erl_interface.h"
#include "ei.h"

/*
Node receives messages:
{init, "/path/to/watchfile"} - initialize a thread watching the file

*/

#ifdef DEBUG
#define DO_TRACE 1
#else
#define DO_TRACE 0
#endif

#define TRACE(P) do { if (DO_TRACE) debugf P ; } while (0)
#include <stdarg.h>
static void debugf(char *str, ...)
{
    va_list ap;
    va_start(ap,str);
    fprintf(stderr,"[ecap_node] ");
    vfprintf(stderr,str, ap);
    fprintf(stderr,"\r\n");
    va_end(ap);
}

/********************

Here's how the call graph goes:

ENTRY | msg from vm    | type of msg     | properly-formed | function called
------|----------------|-----------------|-----------------|--------------------
main -+-> process_msg -+-> process_send   -> process_call   -> process_init_cmd 
      |                +-> process_link
      |                +-> process_unlink
      |                +-> process_exit
      +-> (heartbeat)
      +-> process_error

********************/

typedef struct {
    pthread_t* self;
    ETERM* topid;
    char* fname;
    int erlvm_fd;
} info_t;


int setup_connection(int number, char* cookie, short creation, char* nodename);
void process_error();
void process_msg(ErlMessage* emsg, int erl_fd);
void process_send(ErlMessage* emsg, int erl_fd);
void process_call(ETERM* master_pattern, int erl_fd);
void process_init_cmd(ETERM* init_cmd_pattern, int erl_fd, ETERM* msg_frompid);
void process_unlink(ErlMessage* emsg);
void process_link(ErlMessage* emsg);
void process_exit(ErlMessage* emsg);
void watcher(void* data);

int main(int argc, char** argv)
{
    int erl_fd; // FD to the ERL VM
    char* bufp; // storage for incoming messages
    int bufsz = 1024;

    // the following are basically magic values.
    erl_fd = setup_connection(1, "secretcookie", 0, "e1@am335x-evm");

    ErlMessage* emsg;
    emsg = (ErlMessage*)malloc(sizeof(ErlMessage));
    bufp = (char*)malloc(bufsz * sizeof(char));

    while (1) // main loop - we don't ever expect to leave.
    {
        TRACE(("starting main loop"));

        TRACE(("waiting for a message..."));
        int msg_status = erl_receive_msg(erl_fd, bufp, bufsz, emsg);
        TRACE(("got a message"));

        switch (msg_status)
        {
        case (ERL_MSG): 
            TRACE(("message status is ERL_MSG"));
            process_msg(emsg, erl_fd);
            break;
        case (ERL_TICK):
            TRACE(("message status is ERL_TICK"));
            break;
        case (ERL_ERROR):
        default:
            TRACE(("message status is ERL_ERROR"));
            process_error();
            break;
        }

        TRACE(("cleaning up at end of main loop"));
        erl_free_term(emsg->msg);
        erl_free_term(emsg->from);
        erl_free_term(emsg->to);

    } // end while loop

    if (erl_close_connection(erl_fd) < 0)
    {
        perror("failed to close connection. plowing onward.");
    }

    free(emsg);
    free(bufp);

}

// Sets up a connection to the erlang VM. Basically just passes
//  its arguments directly into the relevant erl_connect functions.

int setup_connection(int number, char* cookie, short creation, char* nodename)
{
    int erl_fd;

    erl_init(NULL, 0);
    if(erl_connect_init(number, cookie, creation) == -1)
    {
        erl_err_quit("erl_connect_init failed");
    }
    TRACE(("erl connection initialized"));

    erl_fd = erl_connect(nodename);
    if (erl_fd < 0)
    {
        erl_err_quit("erl_connect failed");
    }
    TRACE(("connected!"));

    return erl_fd;
}

void process_msg(ErlMessage* emsg, int erl_fd)
{
    switch (emsg->type)
    {
    case (ERL_SEND):
    case (ERL_REG_SEND):
        TRACE(("msg type is ERL_[REG_]SEND"));
        process_send(emsg, erl_fd);
        break;
    case (ERL_LINK):
        TRACE(("msg type is ERL_LINK"));
        process_link(emsg);
        break;
    case (ERL_UNLINK):
        TRACE(("msg type is ERL_UNLINK"));
        process_unlink(emsg);
        break;
    case (ERL_EXIT):
    default:
        TRACE(("msg type is ERL_EXIT"));
        process_exit(emsg);
        break;
    }

    TRACE(("leaving process_msg"));
}

void process_send(ErlMessage* emsg, int erl_fd)
{

    ETERM* master_pattern = erl_format("{call, Pid, Cmd}");

    if (erl_match(master_pattern, emsg->msg))
    {
        TRACE(("matched master pattern"));
        process_call(master_pattern, erl_fd);
    }
    else
    {
        erl_err_msg("could not match the master pattern.");
    }

    TRACE(("leaving process_send"));
    erl_free_term(master_pattern);
}

void process_call(ETERM* master_pattern, int erl_fd)
{

    TRACE(("cnode called"));
    ETERM* msg_frompid = erl_var_content(master_pattern, "Pid");
    ETERM* msg_tuple = erl_var_content(master_pattern, "Cmd");
    ETERM* response;

    if (msg_frompid == NULL || msg_tuple == NULL)
    {
        erl_err_msg("could not get Pid or Cmd vars");
        fprintf(stderr, "how do i erl_var_content lol\n");
        return;
    }

    ETERM* init_cmd_pattern = erl_format("{init, Arg}");

    if (erl_match(init_cmd_pattern, msg_tuple))
    {
        TRACE(("matched init command"));
        process_init_cmd(init_cmd_pattern, erl_fd, msg_frompid);
    }
    else
    {
        erl_err_msg("got unknown function call.");
        response = erl_format("{ecap_node, {error, {function_clause, ~w}}}", msg_tuple);
        erl_send(erl_fd, msg_frompid, response);
    }

    TRACE(("cleaning up process call"));
    erl_free_term(response);
    erl_free_term(init_cmd_pattern);
    //erl_free_term(msg_frompid); //this needs to be freed by the watcher, I think
    erl_free_term(msg_tuple);
    TRACE(("leaving process_call"));
}

void process_init_cmd(ETERM* init_cmd_pattern, int erl_fd, ETERM* msg_frompid)
{
    fprintf(stderr, "process_init_cmd got PID "); erl_print_term(stderr, msg_frompid); fprintf(stderr, "\n");

    ETERM* response;
    ETERM* msg_arg = erl_var_content(init_cmd_pattern, "Arg");

    if (!ERL_IS_LIST(msg_arg))
    {
        erl_err_msg("invalid argument to init command");
        response = erl_format("{ecap_node, {error, {badarg, {init, ~w} } } }", msg_arg);
        erl_send(erl_fd, msg_frompid, response);
        goto invarg_cleanup;
    }

    char* fn_str = erl_iolist_to_string(msg_arg);
    if (fn_str == NULL)
    {
        erl_err_msg("could not unpack string");
        response = erl_format("{ecap_node, {error, {badarg, {init, ~w}}}}", msg_arg);
        erl_send(erl_fd, msg_frompid, response);
        goto nostr_cleanup;
    }
    TRACE(("init command got argstring: %s", fn_str));

    pthread_t* thread = (pthread_t*)malloc(sizeof(pthread_t));
    info_t* threadinfo = (info_t*)malloc(sizeof(info_t));

    // We copy all of this data because the transient pointers will be free()d
    //  just before each function returns, destroying in the thread context.
    threadinfo->topid = msg_frompid;
    threadinfo->self  = thread;
    threadinfo->fname = (char*)malloc(strlen(fn_str)+1);
    strncpy(threadinfo->fname, fn_str, strlen(fn_str)+1);
    threadinfo->erlvm_fd = erl_fd;

    int ret;
    if ((ret = pthread_create(thread, NULL, watcher, threadinfo)) != 0)
    {
        TRACE(("failed to create thread, with return code %d", ret));
        erl_err_ret("failed to create watcher thread");
        response = erl_format("{ecap_node, {error, {threadfail}}}");
        erl_send(erl_fd, msg_frompid, response);
        TRACE(("freeing thread stuff"));
        free(thread);
        free(threadinfo->fname);
        free(threadinfo);
        goto nothread_cleanup;
    }

    response = erl_format("{ecap_node, ok}");
    erl_send(erl_fd, msg_frompid, response);

    TRACE(("normal cleanup from init command call"));
nothread_cleanup:
nostr_cleanup:
    TRACE(("freeing fn_str"));
    erl_free(fn_str);
invarg_cleanup:
    TRACE(("freeing terms"));
    erl_free_term(response);
    erl_free_term(msg_arg);
    TRACE(("done with init command processing"));
}

void watcher(void* data)
{
    TRACE(("watcher thread started!"));
    info_t* threadinfo = (info_t*)data;

    int triggerfd, cnt, ret;
    char attrdata[1024];
    struct pollfd ufd;

    if (!ERL_IS_PID(threadinfo->topid))
    {
        fprintf(stderr, "PID is not a pid. wat.");
        pthread_exit(NULL);
    }

    TRACE(("watcher thread got fname(%d): %s", strlen(threadinfo->fname), threadinfo->fname));
    //TRACE(("watcher got PID <.%d.%d> and fd %d", 
    //        ERL_PID_NUMBER(&threadinfo->topid), 
    //        ERL_PID_SERIAL(&threadinfo->topid),
    //        threadinfo->erlvm_fd));
    fprintf(stderr, "watcher got PID "); erl_print_term(stderr, threadinfo->topid); fprintf(stderr, "\n");

    if ((ufd.fd = open(threadinfo->fname, O_RDONLY)) < 0)
    {
        TRACE(("unable to open file %s", threadinfo->fname));
        perror("unable to open file");
        goto end_watcher;
    }

    ufd.events = POLLPRI | POLLERR;

    // dummy read
    cnt = read(ufd.fd, attrdata, 1023);
    TRACE(("dummy read got value %d", cnt));

    TRACE(("starting up main watcher loop"));
    while ((ret = poll(&ufd, 1, 10000)) >= 0)
    {
        TRACE(("began watcher loop"));
        if (ret == 0)
        {
            TRACE(("watcher thread poll() timed out, continuing."));
            continue;
        }
        if (ufd.revents & (POLLPRI|POLLERR))
        {
            TRACE(("watcher caught an event!"));
            close(ufd.fd);
            if ((ufd.fd = open(threadinfo->fname, O_RDONLY)) < 0)
            {
                TRACE(("unable to re-open file %s", threadinfo->fname));
                perror("unable to re-open file");
                break;
            }
            cnt = read(ufd.fd, attrdata, 1023);
            attrdata[cnt] = '\0';
            TRACE(("watcher got %d bytes: %s", cnt, attrdata));

            ETERM* notification = erl_format("{ding, { ~s } }", attrdata);

            int sendret = erl_send(threadinfo->erlvm_fd, threadinfo->topid, notification);
            if (!sendret)
            {
                switch(erl_errno)
                {
                    case(EINVAL):
                        fprintf(stderr, "unable to send notification: bad PID\n");
                        break;
                    case(ENOMEM):
                        fprintf(stderr, "unable to send notification: no memory\n");
                        break;
                    case(EIO):
                        fprintf(stderr, "unable to send notification: I/O error\n");
                        break;
                    default:
                        fprintf(stderr, "unable to send notification: unspecified error\n");
                        break;
                }
            }
            else
            {
                TRACE(("sent notification"));
            }

            free(notification);
        }
        else
        {
            printf("watcher got something weird! revents = %d\n", ufd.revents);
        }
        ufd.revents = 0;
        TRACE(("finishing watcher loop"));
    }

end_watcher:
    erl_free_compound(threadinfo->topid);
    free(threadinfo->fname);
    pthread_t* dying = threadinfo->self;
    free(threadinfo);
    free(dying);
    TRACE(("watcher thread exiting"));
    pthread_exit(NULL);
}

void process_link(ErlMessage* emsg)
{
    TRACE(("link established."));
    // no-op?
}

void process_unlink(ErlMessage* emsg)
{
    TRACE(("link broken. bailing."));
    // probably no-op? maybe quit. let's quit too, eventually.
}

void process_exit(ErlMessage* emsg)
{
    TRACE(("ERL_EXIT or other. bailing.")); // this implies ERL_EXIT or $unknown, so we'll quit.
}

void process_error()
{
    switch (erl_errno)
    {
    case (EMSGSIZE):
        erl_err_sys("buffer too small for message");
        break;
    case (ENOMEM):
        erl_err_sys("out of memory");
        break;
    case (EIO):
        erl_err_sys("io error");
        break;
    default:
        erl_err_sys("unspecified error getting message");
        break;
    }
}
