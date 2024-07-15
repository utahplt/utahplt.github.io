## utahplt.github.io

Our research blog:

<https://utahplt.github.io/>


### How to Contribute

Install Racket and Frog, clone the repo, write a post in Markdown or something, push to deploy.


#### Install Racket

Download:

<https://download.racket-lang.org/>


or build from source:

<https://github.com/racket/racket>


#### Install Frog

Command line:

```
raco pkg install frog
```


#### Clone Blog Repo

```
git clone https://github.com/utahplt/utahplt.github.io
```


#### Write a Post

> From the repo top level (`cd utahplt.github.io/`)

Start from scratch:

```
raco frog --new name-of-my-cool-blog-post
vim ./_src/posts/*my-cool*.md
```

or start from another post:

```
cp _src/posts/2024-02-02-luau-telemetry.md _src/posts/2024-06-15-knockoff-post.md
```


##### Cross-Post

If you want to cross-post to content on another site, use a link as the post title.

**Example:**

```
cat _src/posts/2024-02-02-luau-telemetry.md

    Title: [Privacy-Respecting Type Error Telemetry at Scale](https://blog.brownplt.org/2024/02/02/privacy-telemetry.html)
    Date: 2024-02-02T01:11:11
    Tags: 

```


#### Build Locally

To build, serve, and automatically open the blog homepage:

```
raco frog -bp
```

To build, serve, and print the homepage URL:

```
raco frog -bs
```

>   Frog 0.30
>   Your Web application is running at http://localhost:3000.
>   Stop this program at any time to terminate the Web Server.
>   ^C
>   Web Server stopped.

Either way, hit control-C to stop the server.


#### Deploy

You must build the blog locally before deploying.

Generate files, commit them all, and push:


```
raco frog -b
git add .
git commit -m "yay newpost"
git push upstream main
```

TODO someday, stop committing generated files.


### Inspiration

<https://blog.brownplt.org/>

<https://prl.khoury.northeastern.edu/blog/index.html>

