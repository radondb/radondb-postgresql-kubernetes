<VirtualHost {{hostText}}>
  ServerAlias *
  DocumentRoot {{documentRoot}}
  <Directory "{{documentRoot}}">
    Options -Indexes +FollowSymLinks -MultiViews
    AllowOverride All
    Require all granted
    DirectoryIndex index.html
  </Directory>

  # Reedirect / to 503 response
  RedirectMatch 503 ^/$

  # Return index.html if server is answering with 404 Not Found status code
  ErrorDocument 404 /index.html
  ErrorDocument 503 /index.html
</VirtualHost>
