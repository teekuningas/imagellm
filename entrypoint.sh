#!/bin/sh

# Trap signals and forward them
trap_handler() {
    echo "Received a signal, propagating to Nginx..."
    kill -s "$1" "$nginx_pid"
}

trap 'trap_handler TERM' TERM;
trap 'trap_handler INT' INT;
trap 'trap_handler QUIT' QUIT;
trap 'trap_handler HUP' HUP;

echo "Replacing env variables in JS files"
for file in /app/dist/index.*.js; do
  if [ -f "$file" ]; then
    sed -i "s|%%RUNTIME_API_ADDRESS%%|${API_ADDRESS:-http://localhost:8000}|g" $file
  fi
done

echo "Starting to serve at 9000"
nginx -g "daemon off;" &
nginx_pid=$!
wait "$nginx_pid"
