sure-deploy
===========

Like Sure Fisk but for deployments.

Build
-----

TODO

Docker API
----------

`curl -G 'http://swarm:2375/v1.24/services' --data-urlencode "filters={\"label\" : {\"com.docker.stack.namespace=stackname\" : true}}"`
`curl -G 'http://swarm:2375/v1.24/services/id'`

Docker image
------------

TODO

License
-------

TODO
