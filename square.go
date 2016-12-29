package main

import (
	"fmt"
	"net/http"
	"strconv"
	"time"
)

func main() {
	http.HandleFunc("/square", func(w http.ResponseWriter, r *http.Request) {
		num := r.URL.Query().Get("num")
		n, _ := strconv.Atoi(num)
		time.Sleep(2 * time.Second)
		fmt.Fprintf(w, "%d", n*n)
	})
	http.ListenAndServe(":9090", nil)
}
