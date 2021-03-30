import React, { useState } from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import { Title } from './Title';
import { Palette } from './Palette';
import { ColourPicker } from './ColourPicker';

const defaultPrimary = 'transparent';
const defaultOthers = [];
const paletteLimit = 2;

const App = () => {
  const [primary, setPrimary] = useState(defaultPrimary);
  const [others, setOthers] = useState(defaultOthers);
  const [mode, setMode] = useState('primary');
  const [paletteFull, setPaletteFull] = useState(false)

  const handlePickColour = v => {
    if (v.mode === 'primary') {
      setPrimary(v.value);
      setMode('others');
    } else {
      setOthers(others => [...others, v.value]);
      if (others.length >= paletteLimit) {
        setPaletteFull(true);
      }
    }
  }

  const handleReset = () => {
    setPrimary(defaultPrimary);
    setOthers(defaultOthers);
    setMode('primary');
    setPaletteFull(false)
  }
  

  const handleCopy = () => {
    navigator.clipboard.writeText([primary, ...others].join(','))
      .then(handleReset);
  }

  return (
    <div>
      <Title>Cool palette builder</Title>
      <Palette primary={primary} others={others} />
      <ColourPicker 
        disabled={paletteFull} 
        onReset={handleReset} 
        onPickColour={handlePickColour} 
        mode={mode}
        handleCopy={handleCopy}
      />
    </div> 
  )
};
  

// ========================================

ReactDOM.render(
  <App />,
  document.getElementById('root')
);
