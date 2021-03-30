import React from 'react';
import './palette.css';

export const Palette = ({primary, others = []}) => (<div className="palette">
  <div className="primary" title={primary} style={{backgroundColor: primary}}></div>
  {others.map(
    colour => 
      <div 
        title={colour} 
        className="other" 
        style={{width: `${60 / others.length}%`, backgroundColor: colour}} />
  )}
</div>)