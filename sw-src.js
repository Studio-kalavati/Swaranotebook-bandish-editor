importScripts(
  'https://storage.googleapis.com/workbox-cdn/releases/6.4.1/workbox-sw.js'
);

import {registerRoute} from 'workbox-routing';
import {StaleWhileRevalidate} from 'workbox-strategies';
import {precacheAndRoute} from 'workbox-precaching';
import {NetworkFirst} from 'workbox-strategies';

precacheAndRoute(self.__WB_MANIFEST);

registerRoute(/\.(?:png|gif|jpg|jpeg|woff|woff2|eot|ttf|svg)$/, new CacheFirst());

registerRoute(
  ({url}) => url.pathname.startsWith('/view/'),
  new NetworkFirst()
);

registerRoute(
  /\.(?:js|html|css)$/,
  new StaleWhileRevalidate()
);
