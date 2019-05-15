import React, { useState } from 'react';
import ReactFileReader from 'react-file-reader';

import './App.css';
function App() {
  const [x, setX] = useState([]);
  const [y, setY] = useState([]);
  const [integer, setInteger] = useState(0);
  const [order, setOrder] = useState(0);

  function handleFiles(files) {
    var reader = new FileReader();
    var row
    var xTemp = []
    var yTemp = []
    reader.onload = function (e) {
      row = reader.result.split('\n')
      setOrder(row[0].split(',')[2])
      setInteger(row[0].split(',')[3])
      row.forEach(item => {
        xTemp.push(item.split(',')[0])
        yTemp.push(item.split(',')[1])
      })
      setX(xTemp)
      setY(yTemp)
    }
    reader.readAsText(files[0]);
  }
  return (
    < div className="App" >
      <div className="Nav">
        <ul className="NavElements">
          <li><a href="default.asp">Simplex</a></li>
          <li><a href="news.asp">Quadratic Spline Interpolation</a></li>
          <li><a href="news.asp">Polynomial Regression</a></li>
        </ul>
      </div>
      <div>
        <ReactFileReader handleFiles={handleFiles} fileTypes={'.csv'}>
          <button className='btn'>Upload</button>
        </ReactFileReader>
      </div>
      <div>
        <p> {integer} </p>
        <p> {order} </p>
      </div>
    </div >
  );
}

export default App;
