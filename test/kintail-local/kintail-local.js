if (self instanceof Window) {
  console.log('Running as main script')
  if ('serviceWorker' in navigator) {
    console.log('Registering service worker')
    navigator.serviceWorker.register(document.currentScript.src).then(function(registration) {
      console.log('ServiceWorker registration successful with scope: ', registration.scope);
    }).catch(function(err) {
      console.log('ServiceWorker registration failed: ', err);
    });
  } else {
    console.log('Service workers are not supported')
  }
} else if (self instanceof ServiceWorkerGlobalScope) {
  console.log('Running as service worker')
  self.addEventListener('fetch', function(event) {
    console.log('Intercepting fetch event for ' + event.request.url)
  });
}
