if ('window' in self) {
  if ('serviceWorker' in navigator) {
    self['Kintail'] = {}
    Kintail['Local'] = {}
    Kintail.Local['init'] = (function(thisFile) {
      function badRequest(description) {
        return Promise.resolve({status: 400, statusText: 'Bad Request', body: description})
      }

      function notFound(description) {
        return Promise.resolve({status: 404, statusText: 'Not Found', body: description})
      }

      function text(body) {
        return Promise.resolve({status: 200, statusText: 'OK', body: body})
      }

      function ok(body) {
        return Promise.resolve({status: 200, statusText: 'OK', body: body})
      }

      function readFile(body) {
        var requestedFile = JSON.parse(body)

        var fileElement = document.getElementById(requestedFile.elementId)
        if (fileElement === null) {
          return notFound('Could not find <file> element')
        }

        var index = requestedFile.index
        if (index < 0 || index >= fileElement.files.size) {
          return notFound('Invalid index for given <file> element')
        }        
        
        var file = fileElement.files[index]
        if (file.name !== requestedFile.name) {
          return notFound('File name does not match')
        }
        if (file.size !== requestedFile.size) {
          return notFound('File size does not match')
        }
        if (file.lastModified !== requestedFile.lastModified) {
          return notFound('File last-modified time does not match')
        }
        if (file.type !== requestedFile.mimeType) {
          return notFound('File MIME type does not match')
        }

        return new Promise(function(resolve, reject) {
          var reader = new FileReader();

          reader.onload = function(event) {
            resolve(reader.result)
          }

          reader.readAsText(file);
        }).then(function(text) {
          return ok(text)
        })
      }

      function handleRequest(request) {
        if (request.path == 'file/read') {
          return readFile(request.body)
        } else {
          return badRequest("Unrecognized request")
        }
      }

      function registerServiceWorker() {
        return navigator.serviceWorker.register(thisFile).then(function(registration) {
          navigator.serviceWorker.addEventListener('message', function(event) {
            console.log('Received message from service worker', event.data)
            handleRequest(event.data).then(function(response) {
              console.log('Sending message back to service worker', response)
              event.ports[0].postMessage(response)
            })
          })
        }).catch(function(err) {
          console.log('ServiceWorker registration failed: ', err)
        })
      }

      return function() {
        return navigator.serviceWorker.getRegistration().then(function(registration) {
          if (registration === undefined) {
            return registerServiceWorker()
          } else {
            return registration.unregister().then(registerServiceWorker)
          }
        })
      }
    })(document.currentScript.src)
  } else {
    console.log('Service workers are not supported')
  }
} else if (self instanceof ServiceWorkerGlobalScope) {
  self.addEventListener('install', function(event) {
    event.waitUntil(self.skipWaiting())
  })

  self.addEventListener('activate', function(event) {
    event.waitUntil(self.clients.claim())
  })

  function requestFromClient(clientId, request) {
    return clients.get(clientId).then(function(client) {
      return new Promise(function(resolve, reject) {
        if (client !== undefined) {
          var messageChannel = new MessageChannel()

          messageChannel.port1.onmessage = function(event) {
            resolve(event.data)
          }
          
          console.log('Posting request to client', request)

          client.postMessage(request, [messageChannel.port2])
        } else {
          reject('Client not found')
        }
      })
    })
  }

  self.addEventListener('fetch', function(event) {
    var request = event.request
    var url = request.url
    if (url.startsWith('https://kintail/local/')) {
      var path = url.slice(22)
      event.respondWith(request.text().then(function(body) {
        return requestFromClient(event.clientId, {path: path, body: body}).then(function(response) {
          var headers = new Headers();
          headers.append('Content-Type', response.contentType);
          return new Response(response.body, {status: response.status, statusText: response.statusText, headers: headers})
        })
      }))
    } else {
      event.respondWith(fetch(event.request))
    }
  })
}
