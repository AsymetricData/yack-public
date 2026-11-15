#define _GNU_SOURCE
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/resource.h>

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: peakrss <binary>\n");
        return 2;
    }

    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        return 2;
    }
    if (pid == 0) {
        freopen("/dev/null", "w", stdout);
        execl(argv[1], argv[1], (char *)NULL);
        perror("exec");
        _exit(127);
    }

    int status = 0;
    struct rusage usage;
    if (wait4(pid, &status, 0, &usage) < 0) {
        perror("wait4");
        return 2;
    }
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        fprintf(stderr, "peakrss: child failed\n");
        return 1;
    }

    printf("%ld\n", usage.ru_maxrss);
    return 0;
}
