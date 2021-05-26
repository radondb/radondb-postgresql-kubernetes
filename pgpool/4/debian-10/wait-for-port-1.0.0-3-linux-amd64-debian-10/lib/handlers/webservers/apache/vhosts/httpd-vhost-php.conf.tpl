AddType application/x-httpd-php .php
{{phpFpmProxy}}
<VirtualHost {{hostText}}>
  ServerAlias *
  {{sslInfo}}
  DocumentRoot {{documentRoot}}
  <Directory "{{documentRoot}}">
    Options -Indexes +FollowSymLinks -MultiViews
    AllowOverride {{allowOverride}}
    Require {{requireOption}}
    DirectoryIndex index.html index.php
    {{phpFpmHandler}}
    {{extraDirectoryConfiguration}}
  </Directory>
  {{additionalConfiguration}}
  {{htaccess}}
</VirtualHost>
