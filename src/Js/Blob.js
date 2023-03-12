export function typeImpl(blob) {
  return blob.type;
}

const fromSources = function (sources, options) {
  if (options === null) {
    return new Blob(sources);
  } else {
    return new Blob(sources, options);
  }
};

export const fromStringsImpl = function (sources) {
  return function (options) {
    return fromSources(sources, options);
  };
};

export function size(blob) {
  return blob.size;
}

export function sliceImpl(contentType) {
  return function (start) {
    return function (end) {
      return function (blob) {
        if (contentType != null) {
          return blob.slice(start, end, contentType);
        } else {
          return blob.slice(start, end);
        }
      };
    };
  };
}

export const text = function (blob) {
  return function () {
    return blob.text();
  };
};

export const toArrayBuffer = function (blob) {
  return function () {
    return blob.arrayBuffer();
  };
};

export const fromArrayBuffersImpl = function (sources) {
  return function (options) {
    return fromSources(sources, options);
  };
};

export const fromBlobsImpl = function (sources) {
  return function (options) {
    return fromSources(sources, options);
  };
};

export const fromDataViewImpl = function (sources) {
  return function (options) {
    return fromSources(sources, options);
  };
};
