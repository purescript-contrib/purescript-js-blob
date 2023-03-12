
export const testDataView = () => {
  const buffer = new ArrayBuffer(16);
  return new DataView(buffer, 2, 10);
};
