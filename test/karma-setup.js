// Ensure global is defined in the browser environment
if (typeof window !== 'undefined' && typeof global === 'undefined') {
  window.global = window;
  console.log('Karma setup: Added global polyfill for browser environment');
}
