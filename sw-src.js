importScripts(
  'https://storage.googleapis.com/workbox-cdn/releases/6.4.1/workbox-sw.js'
);

workbox.precaching.precacheAndRoute(self.__WB_MANIFEST);

workbox.routing.registerRoute(/\.(?:png|gif|jpg|jpeg|woff|woff2|eot|ttf|svg)$/, workbox.strategies.cacheFirst());

workbox.routing.registerRoute(
  ({url}) => url.pathname.startsWith('/view/'),
  workbox.strategies.networkFirst()
);

workbox.routing.registerRoute(
  /\.(?:js|html|css)$/,
  workbox.strategies.staleWhileRevalidate()
);
