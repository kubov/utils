#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <unistd.h>
#include <strings.h>
#include <netdb.h>
#include <signal.h>

#define UP      0x46
#define DOWN    0x15
#define RIGHT   0x43
#define LEFT    0x44
#define OK      0x40
#define ZERO    0x52
#define ONE     0x16
#define TWO     0x19
#define THREE   0xd
#define FOUR    0xc
#define FIVE    0x18
#define SIX     0x5e
#define SEVEN   0x8
#define EIGHT   0x1c
#define NINE    0x5a
#define ASTERIX 0x42
#define HASH    0x4a

char keep_going = 1;
int sock;

typedef struct ir_command
{
    unsigned int high_addr;
    unsigned int low_addr;
    unsigned short int lol_wut;
    unsigned short int key_code;
    unsigned char state;
    char align[3];
}ir_command;

int try_connect(int port)
{
    sock = socket(AF_INET, SOCK_STREAM, 0);
    struct hostent *server = gethostbyname("mustafar");
    struct sockaddr_in serv_addr;
    bzero(&serv_addr, sizeof(struct sockaddr_in));
    serv_addr.sin_family = AF_INET;

    bcopy((char *)server->h_addr, 
          (char *)&serv_addr.sin_addr.s_addr,
          server->h_length);
    
    serv_addr.sin_port = htons(port);

    if (connect(sock,(struct sockaddr *) &serv_addr,sizeof(serv_addr)) == 0)
    {
        puts("log: connected");
        return 1;
    }
    else
    {
        puts("log: connect failed, retry in 1sec");
        return 0;
    }
}

void catch_brokenpipe(int sig)
{
    puts("log: broken pipe!");
    keep_going = 0;
    signal(sig, catch_brokenpipe);
}

void loop(FILE* f)
{
    ir_command cmd;
    char buffer[1024];


    char* names[0xff];
    names[0x46] = "UP"   ;names[0x0c] = "FOUR";
    names[0x15] = "DOWN" ;names[0x18] = "FIVE";
    names[0x43] = "RIGHT";names[0x5e] = "SIX";
    names[0x44] = "LEFT" ;names[0x08] = "SEVEN";
    names[0x40] = "OK"   ;names[0x1c] = "EIGHT";
    names[0x52] = "ZERO" ;names[0x5a] = "NINE";
    names[0x16] = "ONE"  ;names[0x42] = "ASTERIX";
    names[0x19] = "TWO"  ;names[0x4a] = "HASH";
    names[0x0d] = "THREE";names[0x00] = "NONE";

    char* state[] = {"UP", "DOWN"};

    while(keep_going)
    {
        fread(&cmd, sizeof(ir_command), 1, f);

        if (cmd.key_code == 0)
            continue;

        sprintf(buffer, "(%s . %s)\n",
               names[cmd.key_code],
               state[cmd.state]);
        write(sock, buffer, strlen(buffer));
    }
}

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        puts("gimme file as arg, dawg");
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");

    if (f == NULL)
    {
        puts("EEE...wrong file!");
        return 2;
    }

    signal(SIGPIPE, catch_brokenpipe);
    
    while(1)
    {
        if (try_connect(atoi(argv[2])))
        {
            keep_going = 1;
            loop(f);
        }
        sleep(1);
    }
    
    return 0;
}


