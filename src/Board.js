import React from 'react';
import Square from './Square';
import Clue from './Clue';

class Board extends React.Component {
    /**
     * Retorna un número el cual es el mayor de los tamaños de las todas las listas. 
     * @param {*} ListaPistas Lista, la cual tiene listas de pistas.
     * @returns Mayor, siendo mayor el tamaño de la lista más grande.
     */
    alturaPistas(ListaPistas) {
        let mayor = 0;  
        for (let i = 0; i < ListaPistas.length; i++) {
                if (ListaPistas[i].length > mayor)
                    mayor = ListaPistas[i].length;
        }
        return mayor;
    }

    render() {
        const numOfRows = this.props.grid.length;
        const numOfCols = this.props.grid[0].length;
        const { satisfaccionFil, satisfaccionCol } = this.props;
        const rowClues = this.props.rowClues;
        const colClues = this.props.colClues;
        const clueHigh = Math.max(this.alturaPistas(rowClues)* 15, 60);//Actualiza el valor a setear en la parte grafica para que que la fila se actualice en tamaño dinamicamente.
        const colclueHigh = Math.max(this.alturaPistas(colClues)* 25, 60);//Actualiza el valor a setear en la parte grafica para que que la columna se actualice en tamaño dinamicamente.



        return (

            <div className="vertical">
                <div
                    className="colClues"
                    style={{
                        gridTemplateRows: colclueHigh + 'px',
                        gridTemplateColumns: '60px repeat(' + numOfCols + ', 40px)'
                        /*
                           60px  40px 40px 40px 40px 40px 40px 40px   (gridTemplateColumns)
                          ______ ____ ____ ____ ____ ____ ____ ____
                         |      |    |    |    |    |    |    |    |  60px
                         |      |    |    |    |    |    |    |    |  (gridTemplateRows)
                          ------ ---- ---- ---- ---- ---- ---- ---- 
                         */
                    }}
                >
                    <div>{/* top-left corner square */}</div>
                    {colClues.map((clue, i) =>
                        <Clue clue={clue} key={i} satisface={satisfaccionCol[i]} index={i} />
                    )}
                </div>
                <div className="horizontal">
                    <div
                        className="rowClues"
                        style={{
                            gridTemplateRows: 'repeat(' + numOfRows + ', 40px)', 
                            gridTemplateColumns:  clueHigh + 'px',
                            /* IDEM column clues above */
                        }}
                    >
                        {rowClues.map((clue, i) =>
                            <Clue clue={clue} key={i} satisface={satisfaccionFil[i]} index={i} />
                        )}
                    </div>
                    <div className="board"
                        style={{
                            gridTemplateRows: 'repeat(' + numOfRows + ', 40px)',
                            gridTemplateColumns: 'repeat(' + numOfCols + ', 40px)'
                        }}>
                        {this.props.grid.map((row, i) =>
                            row.map((cell, j) =>
                                <Square
                                    value={cell}
                                    onClick={() => this.props.onClick(i, j)}
                                    key={i + j}
                                />
                            )
                        )}
                    </div>
                </div>
            </div>
        );
    }
}

export default Board;