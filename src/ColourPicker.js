import React, { useState } from 'react';
import './colourpicker.css';
import {GithubPicker} from 'react-color';


const defaultColour = '#fff'

// mode = primary or other
export const ColourPicker = ({onPickColour, mode = 'primary', onReset, disabled, handleCopy}) => {
  const [showPicker, setShowPicker ] = useState(false);
  const [colour, setColour] = useState(defaultColour);

  const handleClick = () => setShowPicker(v => !v);
  const handlePickColour = c => {
    setColour(c.hex);
    onPickColour({mode: mode, value: c.hex});
    setShowPicker(false);
    setColour(defaultColour);
  }

  
  return (
    <div>
    <div style={{display: 'flex', flexDirection: 'row', justifyContent: 'flex-end'}}>
      {disabled 
        ? <button className="save colour-picker" onClick={handleCopy}>Copy</button> 
        : <button disabled={disabled} type="button" className="colour-picker" onClick={handleClick}>
            {`Choose ${mode === 'primary' ? 'primary' : 'other'} colour`}
          </button>
      }
      <button type="button" className="reset" onClick={onReset}>Reset</button>
    </div>
    {showPicker && <div className="picker-container"><GithubPicker color={colour} onChangeComplete={handlePickColour} /></div>}
    </div>
  );
}