check process {{service}}
  with pidfile "{{pidFile}}"
  start program = "{{namiPath}} --nami-prefix {{home}}/.nami start {{service}}" with timeout 90 seconds
  stop program = "{{namiPath}} --nami-prefix {{home}}/.nami stop {{service}}" with timeout 90 seconds
