package main

import (
	"flag"
	"fmt"
)

func main() {
	name := flag.String("name", "Yack", "who to greet")
	flag.Parse()
	fmt.Println("Hello from " + *name + " !")
}
