#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>

int main()
{
    int fd;
    char * myfifo = "capture_file";
    char buffer[100];
    int counter = 0; 
    printf("generating traffic...\n");

    /* create the FIFO (named pipe) */
    mkfifo(myfifo, 0666);
    printf("made fifo %s\n", myfifo);

    /* write "Hi" to the FIFO */
    fd = open(myfifo, O_WRONLY);
    printf("opened.\n");
    for (;;sleep(15))
    {
        int len = snprintf(buffer, 99, "%x", (1 << (counter+1))|(1<<(((counter+2)%4)+1)));
        write(fd, buffer, len); 
        counter = (counter+1) % 4;
    }
    printf("\nclosing. ");
    close(fd);

    /* remove the FIFO */
    printf("exiting.");
    unlink(myfifo);

    return 0;
}
