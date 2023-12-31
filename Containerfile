# Use an Nginx base image
FROM debian:latest

# Install deps
RUN apt-get update && \
    apt-get install -y nodejs npm nginx gzip procps && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /app

# Copy the source code
COPY package.json package-lock.json elm.json index.html index.js .parcelrc .npmrc ./
COPY src/ ./src/
COPY static/ ./static/
COPY styles/ ./styles/

# Install dependencies
RUN npm install

# Build the application
RUN ./node_modules/.bin/parcel build index.html

# Define the inline nginx.conf
RUN echo '\
server { \n\
  listen       9000; \n\
  server_name  localhost; \n\
  location / { \n\
    root   /app/dist; \n\
    index  index.html index.htm; \n\
    try_files $uri $uri/ /index.html; \n\
  } \n\
} \
' > /etc/nginx/conf.d/default.conf

# Define the entrypoint
COPY entrypoint.sh /entrypoint.sh

# Expose port 9000
EXPOSE 9000

# Start
CMD ["/entrypoint.sh"]
